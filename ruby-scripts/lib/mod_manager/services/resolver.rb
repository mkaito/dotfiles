# frozen_string_literal: true

require "mod_manager/errors"

module ModManager
  module Services
    # modset       - Modset (responds to .collections, .mods, .path)
    # collections  - Hash<name, Collection> — already loaded
    # archive      - responds to .latest(slug) → Mod | nil
    module Resolver
      def self.resolve(modset, collections, archive)
        key_order = []
        key_to_slug = {}
        key_slugs = {}

        scan = lambda do |slug, ref|
          mod = archive.latest(slug) or raise Error, "mod not in archive: #{slug.inspect}\n  referenced from #{ref}\n  run `mod list` to see available mods"
          key = mod_key(mod)
          key_order << key unless key_to_slug.key?(key)
          key_to_slug[key] = slug
          (key_slugs[key] ||= []) << slug
        end

        modset.collections.each do |col_name|
          col = collections[col_name] or raise Error, "collection not found: #{col_name}"
          col.mods.each { scan.call(it, col_name) }
        end

        modset.mods.each { scan.call(it, modset.path.to_s) }

        mods = key_order.map { archive.latest(key_to_slug[it]) }
        conflicts = key_slugs.select { |_, slugs| slugs.uniq.size > 1 }
        [mods, conflicts]
      end

      def self.mod_key(mod)
        src = mod.source
        return "slug:#{mod.slug}" if src.nil? || src.empty? || src["mod_id"].nil?
        "#{src["provider"]}:#{src["mod_id"]}"
      end
      private_class_method :mod_key
    end
  end
end
