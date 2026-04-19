# frozen_string_literal: true

module ModManager
  module Services
    module GameProfile
      module Cyberpunk2077
        # Files the game engine writes at runtime into deployed directories.
        # Used by link-farm redirect mechanism to intercept writes through dir symlinks.
        module Redirects
          def self.filenames_for(_dst_rel) = %w[db.sqlite3]
        end

        SQLITE_PATTERN = /\.sqlite3\z/
        EPHEMERAL_PATTERN = /(?:\.log.?|final\.redscripts\.(?:modded|ts))\z|vkd3d-proton/
        private_constant :SQLITE_PATTERN, :EPHEMERAL_PATTERN

        # Classifies a real file found in the game directory.
        # rel_path is relative to game_dir.
        #
        # :stateful  — the game writes to this file at runtime; preserve it across deploys
        # :ephemeral — transient noise (logs, caches); discard before and after deploy
        # :static    — ordinary file; leave it alone
        #
        # CET config files (bindings.json, config.json, etc.) are intentionally :static.
        # CET replaces symlinks with real files and recreates from templates, so we leave
        # them alone and let CET manage them entirely.
        def self.cleanup_action(rel_path)
          base = File.basename(rel_path)
          if base.match?(SQLITE_PATTERN)
            :stateful
          elsif base.match?(EPHEMERAL_PATTERN)
            :ephemeral
          else
            :static
          end
        end
      end
    end
  end
end
