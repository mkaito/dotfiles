# frozen_string_literal: true

require "mod_manager/errors"

module ModManager
  module Services
    # modset       - Modset (responds to .collections, .mods, .path)
    # collections  - Hash<name, Collection> — already loaded
    # archive      - responds to .latest(slug) → Mod | nil
    module Resolver
      def self.resolve(modset, collections, archive)
        seen = {}
        mods = []

        scan = lambda do |slug, ref|
          mod = archive.latest(slug) or raise Error, "mod not in archive: #{slug.inspect}\n  referenced from #{ref}\n  run `mod list` to see available mods"
          unless seen[slug]
            seen[slug] = true
            mods << mod
          end
        end

        modset.collections.each do |col_name|
          col = collections[col_name] or raise Error, "collection not found: #{col_name}"
          col.mods.each { scan.call(it, col_name) }
        end

        modset.mods.each { scan.call(it, modset.path.to_s) }

        mods
      end
    end
  end
end
