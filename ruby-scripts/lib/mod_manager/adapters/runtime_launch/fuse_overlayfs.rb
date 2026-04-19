# frozen_string_literal: true

require "fileutils"
require "shellwords"
require "mod_manager/errors"
require "mod_manager/log"

module ModManager
  module Adapters
    module RuntimeLaunch
      class FuseOverlayfs
        FUSE_OVERLAYFS = "fuse-overlayfs"
        FUSERMOUNT = "fusermount"
        UNSHARE = "unshare"

        def initialize(game_dir:, data_dir:)
          @game_dir = File.expand_path(game_dir)
          @data_dir = data_dir
        end

        def launch(mods:, modset:, command:)
          validate!(mods)

          merged = File.join(@data_dir, modset, "merged")
          upper  = File.join(@data_dir, modset, "upper")
          work   = File.join(@data_dir, modset, "work")

          FileUtils.mkdir_p([merged, upper, work])

          lowerdir = (mods.map(&:path) + [@game_dir]).join(":")
          options  = "lowerdir=#{lowerdir},upperdir=#{upper},workdir=#{work}"
          script   = build_script(options, merged, @game_dir)

          Log.debug("launching with overlayfs (#{mods.size} mods, modset=#{modset})")
          pid = spawn(UNSHARE, "--mount", "--map-root-user", "sh", "-c", script, "--", *command)
          _, status = Process.wait2(pid)
          {exit_code: status.exitstatus}
        end

        private

        def validate!(mods)
          missing_bins = [FUSE_OVERLAYFS, FUSERMOUNT, UNSHARE].reject { which(_1) }
          raise Error, "missing required binaries: #{missing_bins.join(", ")}" if missing_bins.any?

          raise Error, "game_dir not found: #{@game_dir}" unless File.directory?(@game_dir)

          missing_mods = mods.reject { File.directory?(_1.path) }.map(&:path)
          raise Error, "mod paths not found:\n#{missing_mods.map { "  #{_1}" }.join("\n")}" if missing_mods.any?
        end

        def build_script(options, merged, game_dir)
          o = Shellwords.shellescape(options)
          m = Shellwords.shellescape(merged)
          g = Shellwords.shellescape(game_dir)

          <<~SH
            cleanup() { umount #{g} 2>/dev/null; #{FUSERMOUNT} -u #{m} 2>/dev/null; wait "$FUSE_PID" 2>/dev/null; }
            trap cleanup EXIT

            #{FUSE_OVERLAYFS} -f -o #{o} #{m} &
            FUSE_PID=$!

            for i in $(seq 50); do
              mountpoint -q #{m} && break
              sleep 0.1
            done
            mountpoint -q #{m} || { echo "fuse-overlayfs: mount timed out" >&2; exit 1; }

            mount --bind #{m} #{g} || exit 1
            "$@"
          SH
        end

        def which(bin)
          ENV.fetch("PATH", "").split(File::PATH_SEPARATOR).any? do |dir|
            File.executable?(File.join(dir, bin))
          end
        end
      end
    end
  end
end
