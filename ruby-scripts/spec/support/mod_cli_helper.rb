# frozen_string_literal: true

require "bundler/setup"
require "optparse"
require "fileutils"
require "set"
require_relative "../../lib/core/cli"
require_relative "../../lib/core/format"
require_relative "../../lib/mod_manager/errors"
require_relative "../../lib/mod_manager/log"
require_relative "../../lib/mod_manager/config"
require_relative "../../lib/mod_manager/mod"
require_relative "../../lib/mod_manager/archive"
require_relative "../../lib/mod_manager/collection"
require_relative "../../lib/mod_manager/collection_editor"
require_relative "../../lib/mod_manager/modset"
require_relative "../../lib/mod_manager/modset_editor"
require_relative "../../lib/nexus/client"
require_relative "../../lib/nexus/file_picker"
require_relative "../../lib/mod_manager/adapters/terminal/memory"
require_relative "../../lib/mod_manager/cli"

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
