# frozen_string_literal: true

require "fileutils"
require "find"
require "mod_manager/errors"
require "mod_manager/log"
require "mod_manager/services/deploy_tree"

module ModManager
  module Adapters
    module Deploy
      class LinkFarm
        def initialize(game_dir, archive_dir)
          @game_dir    = File.expand_path(game_dir)
          @archive_dir = File.expand_path(archive_dir)
          @data_dir    = File.join(File.dirname(@archive_dir), "mod-data")
        end

        # Sentinel used to mark existing real game files in the deploy tree.
        # Prevents the solver from creating dir symlinks over real game directories.
        EXISTING_PATH_SENTINEL = "__existing__"
        private_constant :EXISTING_PATH_SENTINEL

        # Files matching these patterns are written by mods at runtime and must be
        # redirected to the per-modset data dir so they don't pollute the archive.
        SQLITE_PATTERN    = /\.sqlite3\z/.freeze
        # Files matching these patterns are ephemeral — deleted pre-deploy and on undeploy.
        EPHEMERAL_PATTERN = /(?:\.log.?|final\.redscripts\.(?:modded|ts))\z|vkd3d-proton/.freeze
        # CET writes these config files into its own root dir at runtime.
        # Redirect them so a fresh-install dir symlink doesn't send them into the archive.
        CET_CONFIG_FILES  = %w[bindings.json config.json layout.ini persistent.json].freeze
        private_constant :SQLITE_PATTERN, :EPHEMERAL_PATTERN, :CET_CONFIG_FILES

        def deploy(mods:, modset:)
          pre_deploy_cleanup(modset)

          entries = {}
          mods.each do |mod|
            mod_path_rel = mod.path.delete_prefix(@archive_dir + "/")
            mod.files.each do |src|
              dst_rel = src.delete_prefix("#{mod.path}/")
              Log.debug("file conflict: #{dst_rel} — #{mod.slug} overrides") if entries.key?(dst_rel)
              entries[dst_rel] = mod_path_rel
            end
          end

          real_game_files.each { |rel| entries[rel] ||= EXISTING_PATH_SENTINEL }

          symlinks = Services::DeployTree.solve(
            entries.map { |dst_rel, mod_path_rel| { dst_rel:, mod_path_rel: } }
          )
          symlinks.reject! { _1[:src_rel].start_with?("#{EXISTING_PATH_SENTINEL}/") }

          symlinks.each do |lnk|
            dst = File.join(@game_dir, lnk[:dst_rel])
            next unless File.exist?(dst) || File.symlink?(dst)
            if File.symlink?(dst)
              target = File.expand_path(File.readlink(dst), File.dirname(dst))
              next if target.start_with?(@archive_dir + "/")
              raise Error, "cannot overwrite #{dst}: symlink to non-archive path (#{target})"
            end
            if File.directory?(dst)
              # A real directory is safe to replace with a dir symlink only if it contains
              # nothing but archive symlinks — undeploy will remove them before we create ours.
              blockers = non_archive_content(dst)
              next if blockers.empty?
              raise Error, "cannot overwrite #{dst}: directory has non-archive content (#{blockers.first(3).map { File.basename(_1) }.join(", ")})"
            end
            raise Error, "cannot overwrite #{dst}: not a symlink"
          end

          undeploy

          symlinks.each do |lnk|
            dst = File.join(@game_dir, lnk[:dst_rel])
            src = File.join(@archive_dir, lnk[:src_rel])
            FileUtils.mkdir_p(File.dirname(dst))
            # After undeploy, a now-empty real dir may linger at the dir-symlink target.
            # Pre-flight verified it only had archive content, so it's safe to remove.
            Dir.rmdir(dst) if File.directory?(dst) && !File.symlink?(dst) && Dir.empty?(dst)
            File.symlink(src, dst)
            Log.debug("#{lnk[:dir] ? "link-dir" : "link"} #{dst} -> #{src}")
          end

          dir_symlinks = symlinks.select { _1[:dir] }
          install_data_redirects(dir_symlinks, modset)

          file_links = symlinks.reject { _1[:dir] }.map { File.join(@game_dir, _1[:dst_rel]) }
          missing    = file_links.reject { File.symlink?(_1) }
          dangling   = file_links.select { File.symlink?(_1) && !File.exist?(_1) }

          raise Error, "deploy incomplete — #{missing.size} file(s) not linked:\n  #{missing.first(5).join("\n  ")}" if missing.any?
          raise Error, "#{dangling.size} dangling symlink(s) after deploy:\n  #{dangling.first(5).join("\n  ")}"   if dangling.any?

          { created: symlinks.size }
        end

        # Returns { removed: N }.
        def undeploy
          count = 0
          archive_symlinks.each do |path|
            File.unlink(path)
            count += 1
            Log.debug("unlink #{path}")
            parent = File.dirname(path)
            while parent.start_with?(@game_dir + "/")
              break unless Dir.empty?(parent)
              Dir.rmdir(parent)
              Log.debug("rmdir #{parent}")
              parent = File.dirname(parent)
            end
          end

          # Delete ephemeral files left in game_dir (logs, caches).
          if File.directory?(@game_dir)
            Find.find(@game_dir) do |path|
              Find.prune if File.symlink?(path)
              next unless File.file?(path)
              if File.basename(path).match?(EPHEMERAL_PATTERN)
                File.unlink(path)
                Log.debug("unlink ephemeral #{path}")
              end
            end
          end

          { removed: count }
        end

        def status
          result = Hash.new { |h, k| h[k] = { links: [], broken: [] } }
          archive_symlinks.each do |path|
            target   = File.readlink(path)
            rel      = target.delete_prefix(@archive_dir + "/")
            slug_ver = rel.split("/").first(2).join("/")
            key      = File.exist?(path) ? :links : :broken
            result[slug_ver][key] << path
          end
          result
        end

        def path_present?(rel_path, type)
          full = File.join(@game_dir, rel_path)
          case type
          when "dir"  then File.directory?(full)
          when "file" then File.file?(full)
          else false
          end
        end

        private

        # Migrate runtime-written files to the modset data dir and delete ephemeral files
        # so they don't appear as sentinels and block dir symlinks.
        def pre_deploy_cleanup(modset)
          return unless File.directory?(@game_dir)
          Find.find(@game_dir) do |path|
            Find.prune if File.symlink?(path)
            next unless File.file?(path)
            base = File.basename(path)
            if base.match?(SQLITE_PATTERN) ||
               (CET_CONFIG_FILES.include?(base) && path.include?("/cyber_engine_tweaks/"))
              begin
                dst_rel   = File.dirname(path).delete_prefix(@game_dir + "/")
                data_path = File.join(@data_dir, modset, dst_rel, base)
                FileUtils.mkdir_p(File.dirname(data_path))
                FileUtils.mv(path, data_path) unless File.exist?(data_path)
                File.unlink(path) if File.exist?(path)
                Log.debug("migrated #{path} → #{data_path}")
                prune_empty_dir(File.dirname(path))
              rescue Errno::ENOENT, Errno::EACCES => e
                Log.debug("pre_deploy_cleanup: #{e.message}")
              end
            elsif base.match?(EPHEMERAL_PATTERN)
              begin
                File.unlink(path)
                Log.debug("deleted ephemeral #{path}")
                prune_empty_dir(File.dirname(path))
              rescue Errno::ENOENT, Errno::EACCES => e
                Log.debug("pre_deploy_cleanup: #{e.message}")
              end
            end
          end
        rescue Errno::ENOENT, Errno::EACCES => e
          Log.debug("pre_deploy_cleanup: #{e.message}")
        end

        # For each dir symlink, pre-create a file symlink in the archive for
        # known runtime-written files, redirecting writes to the modset data dir.
        def install_data_redirects(dir_symlinks, modset)
          dir_symlinks.each do |lnk|
            filenames = %w[db.sqlite3]
            filenames += CET_CONFIG_FILES if lnk[:dst_rel].split("/").include?("cyber_engine_tweaks")
            filenames.each do |filename|
              archive_path = File.join(@archive_dir, lnk[:src_rel], filename)
              data_path    = File.join(@data_dir, modset, lnk[:dst_rel], filename)
              ensure_data_redirect(archive_path, data_path)
            end
          end
        end

        # Ensure archive_path is a symlink pointing to data_path.
        # If a real file exists in the archive (written before the redirect was set up),
        # move it to data_path first.
        def ensure_data_redirect(archive_path, data_path)
          if File.symlink?(archive_path)
            target = File.expand_path(File.readlink(archive_path), File.dirname(archive_path))
            return if target == data_path
            File.unlink(archive_path)
          elsif File.exist?(archive_path)
            FileUtils.mkdir_p(File.dirname(data_path))
            FileUtils.mv(archive_path, data_path) unless File.exist?(data_path)
            File.unlink(archive_path) if File.exist?(archive_path)
          end
          FileUtils.mkdir_p(File.dirname(data_path))
          File.symlink(data_path, archive_path)
          Log.debug("data-redirect #{archive_path} -> #{data_path}")
        rescue Errno::ENOENT, Errno::EACCES => e
          Log.debug("ensure_data_redirect: #{e.message}")
        end

        def prune_empty_dir(dir)
          while dir.start_with?(@game_dir + "/")
            break unless Dir.empty?(dir)
            Dir.rmdir(dir)
            dir = File.dirname(dir)
          end
        rescue Errno::ENOENT, Errno::EACCES
          # already gone
        end

        # Returns paths inside dir that block replacement with a dir symlink:
        # real files and symlinks that don't point into the archive.
        # Archive symlinks are safe — undeploy removes them before we create ours.
        def non_archive_content(dir)
          items = []
          Find.find(dir) do |path|
            next if path == dir
            if File.symlink?(path)
              target = File.expand_path(File.readlink(path), File.dirname(path))
              items << path unless target.start_with?(@archive_dir + "/")
              Find.prune
            elsif File.file?(path)
              items << path
            end
          end
          items
        rescue Errno::ENOENT, Errno::EACCES
          []
        end

        def real_game_files
          return [] unless File.directory?(@game_dir)
          files = []
          Find.find(@game_dir) do |path|
            Find.prune if File.symlink?(path)
            next if path == @game_dir
            files << path.delete_prefix(@game_dir + "/") if File.file?(path)
          end
          files
        rescue Errno::ENOENT, Errno::EACCES => e
          Log.debug("real_game_files: #{e.message}")
          []
        end

        def archive_symlinks
          return [] unless File.directory?(@game_dir)
          links = []
          Find.find(@game_dir) do |path|
            next unless File.symlink?(path)
            target = File.expand_path(File.readlink(path), File.dirname(path))
            links << path if target.start_with?(@archive_dir + "/")
          end
          links
        rescue Errno::ENOENT, Errno::EACCES, Errno::ENOTDIR, Errno::EPERM => e
          Log.debug("archive_symlinks: #{e.message}")
          []
        end
      end
    end
  end
end
