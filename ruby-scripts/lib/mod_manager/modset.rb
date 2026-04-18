# frozen_string_literal: true

require "toml-rb"
require "mod_manager/errors"

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

  end
end
