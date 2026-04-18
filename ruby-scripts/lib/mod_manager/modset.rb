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
    #   mods      - ordered list of Mod objects, last-seen slug wins
    #   conflicts - { slug => [source, ...] } for slugs claimed by multiple sources
    #               source is a collection name or nil for direct @mods entries
    def resolve(archive, config)
      slug_order   = []   # first-seen insertion order
      slug_sources = {}   # slug => [source, ...]  (last is winner)

      @collections.each do |col_name|
        col_path = File.join(config.collections_dir, "#{col_name}.toml")
        raise Error, "collection not found: #{col_path}" unless File.exist?(col_path)
        Collection.load(col_path).mods.each do |slug|
          archive.latest(slug) or raise Error, "mod not in archive: #{slug.inspect}\n  referenced from #{col_path}\n  run `mod list` to see available mods"
          slug_order << slug unless slug_sources.key?(slug)
          (slug_sources[slug] ||= []) << col_name
        end
      end

      @mods.each do |slug|
        archive.latest(slug) or raise Error, "mod not in archive: #{slug.inspect}\n  referenced from #{@path}\n  run `mod list` to see available mods"
        slug_order << slug unless slug_sources.key?(slug)
        (slug_sources[slug] ||= []) << nil
      end

      mods      = slug_order.map { archive.latest(_1) }
      conflicts = slug_sources.select { |_, sources| sources.size > 1 }
      [mods, conflicts]
    end
  end
end
