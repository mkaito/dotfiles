# frozen_string_literal: true

require "toml-rb"
require "mod_manager/errors"

module ModManager
  Collection = Struct.new(:name, :mods, :path, keyword_init: true) do
    def self.load(path)
      data = TomlRB.load_file(path)
      errors = []
      errors << "missing name" if data["name"].to_s.strip.empty?
      errors << "mods must be an array" unless data["mods"].is_a?(Array)
      raise ValidationError.new(errors.map { "#{path}: #{it}" }) if errors.any?
      new(name: data["name"], mods: data["mods"], path:).freeze
    end
  end
end
