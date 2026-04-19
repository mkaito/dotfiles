# frozen_string_literal: true

require "toml-rb"
require "mod_manager/errors"

module ModManager
  OS_ARTIFACTS = %w[.DS_Store Thumbs.db desktop.ini].to_set.freeze
  private_constant :OS_ARTIFACTS

  Mod = Struct.new(:slug, :version, :name, :game, :depends, :source, :path, keyword_init: true) do
    def self.load(dir)
      meta = File.join(dir, "meta.toml")
      raise Error, "meta.toml not found: #{meta}" unless File.exist?(meta)
      data = TomlRB.load_file(meta)
      missing = %w[name slug version game].select { data[it].to_s.strip.empty? }
      raise ValidationError.new(missing.map { "#{meta}: missing #{it}" }) if missing.any?
      new(
        slug: data["slug"],
        version: data["version"],
        name: data["name"],
        game: data["game"],
        depends: Array(data["depends"]),
        source: data["source"] || {},
        path: dir
      ).freeze
    end

    def files
      Dir.glob("#{path}/**/*", File::FNM_DOTMATCH)
        .reject { |f|
          File.directory?(f) ||
            f == "#{path}/meta.toml" ||
            f.split("/").include?("__MACOSX") ||
            OS_ARTIFACTS.include?(File.basename(f))
        }
    end

    def to_s = slug
  end
end
