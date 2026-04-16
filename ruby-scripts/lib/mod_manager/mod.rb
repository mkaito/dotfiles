# frozen_string_literal: true

require "toml-rb"
require_relative "errors"

module ModManager
  Mod = Struct.new(:slug, :version, :name, :game, :depends, :source, :path, keyword_init: true) do
    def self.load(dir)
      meta = File.join(dir, "meta.toml")
      raise Error, "meta.toml not found: #{meta}" unless File.exist?(meta)
      data = TomlRB.load_file(meta)
      missing = %w[name slug version game].select { data[_1].to_s.strip.empty? }
      raise ValidationError.new(missing.map { "#{meta}: missing #{_1}" }) if missing.any?
      new(
        slug:    data["slug"],
        version: data["version"],
        name:    data["name"],
        game:    data["game"],
        depends: Array(data["depends"]),
        source:  data["source"] || {},
        path:    dir,
      ).freeze
    end

    def files
      Dir.glob("#{path}/files/**/*", File::FNM_DOTMATCH).reject { |f| File.directory?(f) }
    end

    def to_s = "#{slug}@#{version}"
  end
end
