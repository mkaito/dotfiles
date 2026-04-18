# frozen_string_literal: true

require "bundler/setup"
require "mod_manager/adapters/terminal/memory"
require "mod_manager/cli"

module ModCliHelper
  def run_mod(*args)
    terminal = ModManager::Adapters::Terminal::Memory.new
    with_env("XDG_CONFIG_HOME" => @xdg_config, "XDG_DATA_HOME" => @xdg_data) do
      status = ModManager::CLI.run(args, terminal)
      [status, terminal.output]
    end
  end

  def with_env(vars)
    old = vars.each_with_object({}) { |(k, _), h| h[k] = ENV[k] }
    vars.each { |k, v| ENV[k] = v }
    yield
  ensure
    old.each { |k, v| v.nil? ? ENV.delete(k) : ENV[k] = v }
  end
end
