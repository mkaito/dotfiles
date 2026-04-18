# frozen_string_literal: true

require "mod_manager/mod"

module ModManager
  module Adapters
    module ModArchive
      class Memory
        attr_reader :mods

        def initialize
          @mods = []
        end

        def seed(slug, version: "1.0", name: slug, game: "cp2077", depends: [], source: {}, path: "/fake/#{slug}")
          mod = Mod.new(slug:, version:, name:, game:, depends:, source:, path:).freeze
          @mods << mod
          mod
        end

        def all = @mods.dup

        def include?(slug) = @mods.any? { _1.slug == slug }

        def latest(slug)
          @mods.select { _1.slug == slug }
            .max_by { _1.version.split(".").map(&:to_i) }
        end

        def delete(mod)
          @mods.reject! { _1.slug == mod.slug && _1.version == mod.version }
        end

        def install(unpacked_mod:)
          mod = Mod.new(
            slug:    unpacked_mod.slug,
            version: unpacked_mod.version,
            name:    unpacked_mod.name,
            game:    unpacked_mod.game,
            depends: [],
            source:  unpacked_mod.source,
            path:    "/fake/#{unpacked_mod.slug}/#{unpacked_mod.version}",
          ).freeze
          @mods << mod
          mod
        end

        def invalidate = nil
      end
    end
  end
end
