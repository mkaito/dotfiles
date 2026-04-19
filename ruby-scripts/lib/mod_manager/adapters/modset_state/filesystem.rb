# frozen_string_literal: true

require "fileutils"
require "find"

module ModManager
  module Adapters
    module ModsetState
      class Filesystem
        INTERNAL_DIRS = %w[upper work merged].freeze

        def initialize(data_dir:)
          @data_dir = data_dir
        end

        def summary(modset:)
          base = modset_dir(modset)
          return nil unless File.directory?(base)

          redirect_bytes = sum_bytes(base, exclude_dirs: INTERNAL_DIRS)
          overlay_bytes  = sum_bytes(File.join(base, "upper"))
          {redirect_bytes:, overlay_bytes:}
        end

        def details(modset:)
          base = modset_dir(modset)
          {
            redirect: list_files(base, exclude_dirs: INTERNAL_DIRS, base:),
            overlay:  list_files(File.join(base, "upper"), base: File.join(base, "upper"))
          }
        end

        def wipe(modset:)
          base = modset_dir(modset)
          bytes = sum_bytes(base)
          FileUtils.rm_rf(base)
          {bytes_freed: bytes}
        end

        private

        def modset_dir(modset) = File.join(@data_dir, modset)

        def sum_bytes(dir, exclude_dirs: [])
          return 0 unless File.directory?(dir)
          total = 0
          Find.find(dir) do |path|
            if File.directory?(path)
              Find.prune if exclude_dirs.include?(File.basename(path)) && path != dir
            elsif File.file?(path) && !whiteout?(path)
              total += File.size(path)
            end
          end
          total
        end

        def list_files(dir, exclude_dirs: [], base:)
          return [] unless File.directory?(dir)
          results = []
          Find.find(dir) do |path|
            if File.directory?(path)
              Find.prune if exclude_dirs.include?(File.basename(path)) && path != dir
            elsif File.file?(path) && !whiteout?(path)
              results << {path: path.delete_prefix("#{base}/"), bytes: File.size(path)}
            end
          end
          results.sort_by { _1[:path] }
        end

        def whiteout?(path) = File.basename(path).start_with?(".wh.")
      end
    end
  end
end
