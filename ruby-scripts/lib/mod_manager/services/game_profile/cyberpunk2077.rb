# frozen_string_literal: true

module ModManager
  module Services
    module GameProfile
      module Cyberpunk2077
        SQLITE_PATTERN    = /\.sqlite3\z/
        EPHEMERAL_PATTERN = /(?:\.log.?|final\.redscripts\.(?:modded|ts))\z|vkd3d-proton/
        CET_CONFIG_FILES  = %w[bindings.json config.json layout.ini persistent.json].freeze
        private_constant :SQLITE_PATTERN, :EPHEMERAL_PATTERN, :CET_CONFIG_FILES

        def self.cleanup_action(rel_path)
          base = File.basename(rel_path)
          if base.match?(SQLITE_PATTERN) ||
             (CET_CONFIG_FILES.include?(base) && rel_path.include?("/cyber_engine_tweaks/"))
            :migrate
          elsif base.match?(EPHEMERAL_PATTERN)
            :delete
          else
            :keep
          end
        end

        def self.redirect_filenames_for(dst_rel)
          files = %w[db.sqlite3]
          files += CET_CONFIG_FILES if dst_rel.split("/").include?("cyber_engine_tweaks")
          files
        end
      end
    end
  end
end
