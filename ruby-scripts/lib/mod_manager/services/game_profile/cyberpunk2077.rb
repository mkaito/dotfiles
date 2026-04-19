# frozen_string_literal: true

module ModManager
  module Services
    module GameProfile
      module Cyberpunk2077
        SQLITE_PATTERN = /\.sqlite3\z/
        EPHEMERAL_PATTERN = /(?:\.log.?|final\.redscripts\.(?:modded|ts))\z|vkd3d-proton/
        CET_CONFIG_FILES = %w[bindings.json config.json layout.ini persistent.json].freeze
        private_constant :SQLITE_PATTERN, :EPHEMERAL_PATTERN, :CET_CONFIG_FILES

        # Classifies a real file found in the game directory.
        # rel_path is relative to game_dir.
        #
        # :stateful  — the game writes to this file at runtime; preserve it across deploys
        # :ephemeral — transient noise (logs, caches); discard before and after deploy
        # :static    — ordinary file; leave it alone
        def self.cleanup_action(rel_path)
          base = File.basename(rel_path)
          if base.match?(SQLITE_PATTERN) ||
              (CET_CONFIG_FILES.include?(base) && rel_path.include?("/cyber_engine_tweaks/"))
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
