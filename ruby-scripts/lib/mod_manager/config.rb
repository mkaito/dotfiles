# frozen_string_literal: true

require "toml-rb"
require_relative "../core/errors"
require_relative "../core/xdg"

module ModManager
  class Config
    attr_reader :game_dir, :domain, :checks, :archive_dir, :collections_dir, :modsets_dir

    def self.load
      xdg_config = Core::XDG.config_home
      xdg_data   = Core::XDG.data_home
      path = File.join(xdg_config, "mods", "config.toml")
      raise Error, "config not found: #{path}" unless File.exist?(path)
      data = TomlRB.load_file(path)
      game = data["cp2077"] or raise Error, "config missing [cp2077] section"
      new(data, game, xdg_config, xdg_data).freeze
    end

    private_class_method :new

    def initialize(data, game, xdg_config, xdg_data)
      @game_dir        = File.expand_path(game["game_dir"] || raise(Error, "config missing cp2077.game_dir"))
      @domain          = game["domain"]                    || raise(Error, "config missing cp2077.domain")
      @checks          = Array(game["checks"])
      @archive_dir     = File.expand_path(data["archive_dir"] || File.join(xdg_data, "mods", "archive"))
      @collections_dir = File.join(xdg_config, "mods", "collections")
      @modsets_dir     = File.join(xdg_config, "mods", "modsets")
    end
  end
end
