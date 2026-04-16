# frozen_string_literal: true

require "toml-rb"
require_relative "errors"

module ModManager
  Collection = Struct.new(:name, :game, :mods, :checks, :path, keyword_init: true) do
    def self.load(path)
      data = TomlRB.load_file(path)
      errors = []
      errors << "missing name" if data["name"].to_s.strip.empty?
      errors << "missing game" if data["game"].to_s.strip.empty?
      errors << "mods must be an array" unless data["mods"].is_a?(Array)
      raise ValidationError.new(errors.map { "#{path}: #{_1}" }) if errors.any?
      new(name: data["name"], game: data["game"], mods: data["mods"],
          checks: Array(data["checks"]), path:).freeze
    end
  end
end
