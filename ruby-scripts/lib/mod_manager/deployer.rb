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

    def deploy(mods)
      undeploy
      created = 0

      mods.each do |mod|
        mod.files.each do |src|
          prefix = "#{mod.path}/"
          raise Error, "unexpected file path: #{src}" unless src.start_with?(prefix)
          rel = src.delete_prefix(prefix)
          dst = File.join(@game_dir, rel)
          FileUtils.mkdir_p(File.dirname(dst))
          if File.symlink?(dst)
            File.unlink(dst)
          elsif File.exist?(dst)
            raise Error, "cannot overwrite #{dst}: not a symlink"
          end
          File.symlink(src, dst)
          created += 1
          Log.debug("link #{dst} -> #{src}")
        end
      end

      broken = verify
      Log.debug("#{broken.size} broken symlink(s) after deploy") if broken.any?
      { created:, broken: }
    end

    def undeploy
      count = 0
      archive_symlinks.each do |path|
        File.unlink(path)
        count += 1
        Log.debug("unlink #{path}")
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
    rescue Errno::ENOENT, Errno::EACCES, Errno::ENOTDIR => e
      Log.debug("archive_symlinks: #{e.message}")
      []
    end

    def verify
      archive_symlinks.reject { File.exist?(_1) }
    end
  end
end
