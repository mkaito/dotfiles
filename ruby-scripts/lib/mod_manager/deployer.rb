# frozen_string_literal: true

require "fileutils"
require "find"
require_relative "errors"
require_relative "log"

module ModManager
  class Deployer
    def initialize(game_dir, archive_dir)
      @game_dir    = File.expand_path(game_dir)
      @archive_dir = File.expand_path(archive_dir)
    end

    # Deploys mods as a symlink farm. Raises on any problem.
    # Returns { created: N }.
    #
    # Pipeline:
    #   1. Resolve mods -> file pairs (dst -> src map, last mod wins on file conflict)
    #   2. Pre-flight: every dst must be absent or an archive-owned symlink
    #   3. Clear: remove all archive symlinks under game_dir
    #   4. Deploy: create symlinks
    #   5. Verify: no dangling, no missing
    def deploy(mods)
      # Step 1 — resolve to dst->src map
      dst_to_src = {}
      mods.each do |mod|
        mod.files.each do |src|
          rel = src.delete_prefix("#{mod.path}/")
          dst = File.join(@game_dir, rel)
          Log.debug("file conflict: #{rel} — #{mod.slug} overrides #{dst_to_src[dst]}") if dst_to_src.key?(dst)
          dst_to_src[dst] = src
        end
      end
      pairs = dst_to_src.to_a  # [[dst, src], ...]

      # Step 2 — pre-flight
      pairs.each do |dst, _|
        next unless File.exist?(dst) || File.symlink?(dst)
        if File.symlink?(dst)
          target = File.expand_path(File.readlink(dst), File.dirname(dst))
          next if target.start_with?(@archive_dir + "/")
          raise Error, "cannot overwrite #{dst}: symlink to non-archive path (#{target})"
        end
        raise Error, "cannot overwrite #{dst}: not a symlink"
      end

      # Step 3 — clear
      undeploy

      # Step 4 — deploy
      pairs.each do |dst, src|
        FileUtils.mkdir_p(File.dirname(dst))
        File.symlink(src, dst)
        Log.debug("link #{dst} -> #{src}")
      end

      # Step 5 — verify
      expected = dst_to_src.keys
      missing  = expected.reject { File.symlink?(_1) }
      dangling = expected.select { File.symlink?(_1) && !File.exist?(_1) }

      raise Error, "deploy incomplete — #{missing.size} file(s) not linked:\n  #{missing.first(5).join("\n  ")}" if missing.any?
      raise Error, "#{dangling.size} dangling symlink(s) after deploy:\n  #{dangling.first(5).join("\n  ")}"   if dangling.any?

      { created: pairs.size }
    end

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
      count
    end

    def status
      result = Hash.new { |h, k| h[k] = { links: [], broken: [] } }
      archive_symlinks.each do |path|
        target  = File.readlink(path)
        rel     = target.delete_prefix(@archive_dir + "/")
        slug_ver = rel.split("/").first(2).join("/")
        key = File.exist?(path) ? :links : :broken
        result[slug_ver][key] << path
      end
      result
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
