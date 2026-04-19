# frozen_string_literal: true

require "fileutils"
require "mod_manager/log"

module ModManager
  module Adapters
    module Deploy
      # Manages runtime-redirect symlinks inside the archive directory.
      # A redirect symlink sits at archive_dir/slug/path/filename and points to
      # data_dir/modset/path/filename, intercepting writes the game makes through
      # dir symlinks so they land in the per-modset data dir rather than the archive.
      #
      # This is a LinkFarm-specific mechanism; other deploy strategies handle
      # stateful files differently (e.g. overlayfs captures writes via the upper layer).
      class RedirectStore
        # No-op redirects — used when no game-specific redirect knowledge is wired in.
        module NullRedirects
          def self.filenames_for(_dst_rel) = []
        end

        def initialize(archive_dir, data_dir, redirects: NullRedirects)
          @archive_dir = archive_dir
          @data_dir = data_dir
          @redirects = redirects
        end

        def install(dir_symlinks, modset)
          dir_symlinks.each do |lnk|
            @redirects.filenames_for(lnk[:dst_rel]).each do |filename|
              archive_path = File.join(@archive_dir, lnk[:src_rel], filename)
              data_path = File.join(@data_dir, modset, lnk[:dst_rel], filename)
              ensure_redirect(archive_path, data_path)
            end
          end
        end

        private

        def ensure_redirect(archive_path, data_path)
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
        end
      end
    end
  end
end
