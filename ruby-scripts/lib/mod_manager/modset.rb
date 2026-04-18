# frozen_string_literal: true

require "toml-rb"
require_relative "errors"
require_relative "collection"

module ModManager
  class Modset
    attr_reader :game, :collections, :mods, :checks, :path

    def self.load(path)
      data = TomlRB.load_file(path)
      errors = []
      errors << "missing game" if data["game"].to_s.strip.empty?
      raise ValidationError.new(errors.map { "#{path}: #{_1}" }) if errors.any?
      new(game: data["game"], collections: Array(data["collections"]),
          mods: Array(data["mods"]), checks: Array(data["checks"]), path:)
    end

    def initialize(game:, collections:, mods:, checks:, path:)
      @game = game
      @collections = collections
      @mods = mods
      @checks = checks
      @path = path
    end

    # Returns [mods, conflicts] where:
    #   mods      - ordered Mod list; when the same mod appears multiple times,
    #               last occurrence wins (overlay model)
    #   conflicts - { mod_key => [slug, ...] } for mod_keys with 2+ distinct slugs
    #               mod_key = "provider:mod_id" (e.g. "nexus:12345"), or
    #                         "slug:name" for manually-installed mods without source metadata
    def resolve(archive, config)
      key_order   = []   # first-seen mod_key order (determines position in final list)
      key_to_slug = {}   # mod_key => winning slug (last wins)
      key_slugs   = {}   # mod_key => [all slugs seen, including repeats]

      scan = lambda do |slug, ref|
        mod = archive.latest(slug) or raise Error, "mod not in archive: #{slug.inspect}\n  referenced from #{ref}\n  run `mod list` to see available mods"
        key = mod_key(mod)
        key_order << key unless key_to_slug.key?(key)
        key_to_slug[key] = slug
        (key_slugs[key] ||= []) << slug
      end

      @collections.each do |col_name|
        col_path = File.join(config.collections_dir, "#{col_name}.toml")
        raise Error, "collection not found: #{col_path}" unless File.exist?(col_path)
        Collection.load(col_path).mods.each { scan.call(_1, col_path) }
      end

      @mods.each { scan.call(_1, @path) }

      mods      = key_order.map { archive.latest(key_to_slug[_1]) }
      conflicts = key_slugs.select { |_, slugs| slugs.uniq.size > 1 }
      [mods, conflicts]
    end

    private

    def mod_key(mod)
      src = mod.source
      return "slug:#{mod.slug}" if src.empty? || src["mod_id"].nil?
      "#{src["provider"]}:#{src["mod_id"]}"
    end
  end
end
