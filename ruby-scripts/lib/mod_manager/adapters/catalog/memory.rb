# frozen_string_literal: true

require "mod_manager/collection"
require "mod_manager/modset"
require "mod_manager/errors"

module ModManager
  module Adapters
    module Catalog
      # In-memory catalog for tests. Backed by hashes; no filesystem access.
      class Memory
        attr_reader :collections, :modsets

        def initialize
          @collections = {}   # name → Collection struct
          @modsets     = {}   # name → Modset struct
        end

        # ── collections ──────────────────────────────────────────────────────

        def list_collections           = @collections.keys.sort
        def collection_exist?(name)    = @collections.key?(name)

        def read_collection(name)
          @collections.fetch(name) { raise Error, "collection not found: #{name}" }
        end

        def write_collection(col)
          @collections[col.name] = col
        end

        def delete_collection(name)
          raise Error, "collection not found: #{name}" unless collection_exist?(name)
          @collections.delete(name)
        end

        # ── modsets ───────────────────────────────────────────────────────────

        def list_modsets               = @modsets.keys.sort
        def modset_exist?(name)        = @modsets.key?(name)

        def read_modset(name)
          @modsets.fetch(name) { raise Error, "modset not found: #{name}" }
        end

        # Derives the key from ms#path (basename without extension).
        def write_modset(ms)
          name = File.basename(ms.path.to_s, ".toml")
          @modsets[name.empty? ? ms.game : name] = ms
        end

        def delete_modset(name)
          raise Error, "modset not found: #{name}" unless modset_exist?(name)
          @modsets.delete(name)
        end

        # ── test helpers ──────────────────────────────────────────────────────

        def seed_collection(name, mods: [])
          col = Collection.new(name:, mods:, path: "/fake/#{name}.toml")
          @collections[name] = col
          col
        end

        def seed_modset(name, game: "cyberpunk2077", collections: [], mods: [], checks: [])
          ms = Modset.new(game:, collections:, mods:, checks:, path: "/fake/#{name}.toml")
          @modsets[name] = ms
          ms
        end
      end
    end
  end
end
