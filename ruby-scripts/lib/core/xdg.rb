# frozen_string_literal: true

module Core
  module XDG
    def self.config_home = ENV.fetch("XDG_CONFIG_HOME", File.expand_path("~/.config"))
    def self.data_home   = ENV.fetch("XDG_DATA_HOME",   File.expand_path("~/.local/share"))
    def self.cache_home  = ENV.fetch("XDG_CACHE_HOME",  File.expand_path("~/.cache"))
  end
end
