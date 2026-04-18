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

    def resolve(archive, config)
      seen    = {}
      ordered = []
      loaded_collections = []

      @collections.each do |name|
        col_path = File.join(config.collections_dir, "#{name}.toml")
        raise Error, "collection not found: #{col_path}" unless File.exist?(col_path)
        col = Collection.load(col_path)
        loaded_collections << col
        col.mods.each do |slug|
          mod = archive.latest(slug) or raise Error, "mod not in archive: #{slug.inspect}\n  referenced from #{col_path}\n  run `mod list` to see available mods"
          next if seen[mod.slug]
          seen[mod.slug] = true
          ordered << mod
        end
      end

      @mods.each do |slug|
        mod = archive.latest(slug) or raise Error, "mod not in archive: #{slug.inspect}\n  referenced from #{@path}\n  run `mod list` to see available mods"
        next if seen[mod.slug]
        seen[mod.slug] = true
        ordered << mod
      end

      [ordered, loaded_collections]
    end
  end
end
