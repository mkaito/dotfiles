# frozen_string_literal: true

require "fileutils"
require "find"
require_relative "../../errors"
require_relative "../../log"

module ModManager
  module Adapters
    module Deploy
      class LinkFarm
        def initialize(game_dir, archive_dir)
          @game_dir    = File.expand_path(game_dir)
          @archive_dir = File.expand_path(archive_dir)
        end

        def deploy(mods:)
          dst_to_src = {}
          mods.each do |mod|
            mod.files.each do |src|
              rel = src.delete_prefix("#{mod.path}/")
              dst = File.join(@game_dir, rel)
              Log.debug("file conflict: #{rel} — #{mod.slug} overrides #{dst_to_src[dst]}") if dst_to_src.key?(dst)
              dst_to_src[dst] = src
            end
          end
          pairs = dst_to_src.to_a

          pairs.each do |dst, _|
            next unless File.exist?(dst) || File.symlink?(dst)
            if File.symlink?(dst)
              target = File.expand_path(File.readlink(dst), File.dirname(dst))
              next if target.start_with?(@archive_dir + "/")
              raise Error, "cannot overwrite #{dst}: symlink to non-archive path (#{target})"
            end
            raise Error, "cannot overwrite #{dst}: not a symlink"
          end

          undeploy

          pairs.each do |dst, src|
            FileUtils.mkdir_p(File.dirname(dst))
            File.symlink(src, dst)
            Log.debug("link #{dst} -> #{src}")
          end

          expected = dst_to_src.keys
          missing  = expected.reject { File.symlink?(_1) }
          dangling = expected.select { File.symlink?(_1) && !File.exist?(_1) }

          raise Error, "deploy incomplete — #{missing.size} file(s) not linked:\n  #{missing.first(5).join("\n  ")}" if missing.any?
          raise Error, "#{dangling.size} dangling symlink(s) after deploy:\n  #{dangling.first(5).join("\n  ")}"   if dangling.any?

          { created: pairs.size }
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
