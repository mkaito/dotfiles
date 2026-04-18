# frozen_string_literal: true

require "minitest/autorun"
require "core/xdg"

class CoreXdgTest < Minitest::Test
  def test_config_home_env_override
    with_env("XDG_CONFIG_HOME" => "/custom/config") do
      assert_equal "/custom/config", Core::XDG.config_home
    end
  end

  def test_config_home_default
    with_env("XDG_CONFIG_HOME" => nil) do
      assert_equal File.expand_path("~/.config"), Core::XDG.config_home
    end
  end

  def test_data_home_env_override
    with_env("XDG_DATA_HOME" => "/custom/data") do
      assert_equal "/custom/data", Core::XDG.data_home
    end
  end

  def test_cache_home_env_override
    with_env("XDG_CACHE_HOME" => "/custom/cache") do
      assert_equal "/custom/cache", Core::XDG.cache_home
    end
  end

  private

  def with_env(vars)
    saved = vars.keys.each_with_object({}) { |k, h| h[k] = ENV[k] }
    vars.each { |k, v| v.nil? ? ENV.delete(k) : ENV[k] = v }
    yield
  ensure
    saved.each { |k, v| v.nil? ? ENV.delete(k) : ENV[k] = v }
  end
end
