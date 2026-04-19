# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/services/game_profile/cyberpunk2077"

include ModManager::Services::GameProfile

class Cyberpunk2077CleanupActionTest < Minitest::Test
  # :stateful — engine writes here at runtime; preserve across deploys
  def test_sqlite3_anywhere_is_stateful
    assert_equal :stateful, Cyberpunk2077.cleanup_action("some/dir/db.sqlite3")
  end

  # CET config files are left as real files — CET replaces symlinks with templates
  def test_bindings_json_in_cet_is_static
    assert_equal :static, Cyberpunk2077.cleanup_action("bin/x64/plugins/cyber_engine_tweaks/bindings.json")
  end

  def test_config_json_in_cet_is_static
    assert_equal :static, Cyberpunk2077.cleanup_action("bin/x64/plugins/cyber_engine_tweaks/config.json")
  end

  # :ephemeral — transient noise; discard
  def test_log_extension_is_ephemeral
    assert_equal :ephemeral, Cyberpunk2077.cleanup_action("bin/x64/plugin.log")
  end

  def test_log_variant_is_ephemeral
    assert_equal :ephemeral, Cyberpunk2077.cleanup_action("bin/x64/Plugin.log?")
  end

  def test_vkd3d_proton_is_ephemeral
    assert_equal :ephemeral, Cyberpunk2077.cleanup_action("bin/x64/vkd3d-proton.cache.write")
  end

  def test_final_redscripts_modded_is_ephemeral
    assert_equal :ephemeral, Cyberpunk2077.cleanup_action("r6/cache/final.redscripts.modded")
  end

  def test_final_redscripts_ts_is_ephemeral
    assert_equal :ephemeral, Cyberpunk2077.cleanup_action("r6/cache/final.redscripts.ts")
  end

  # :static — ordinary file; leave alone
  def test_bindings_json_outside_cet_is_static
    assert_equal :static, Cyberpunk2077.cleanup_action("mods/bindings.json")
  end

  def test_shader_final_cache_is_static
    assert_equal :static, Cyberpunk2077.cleanup_action("bin/x64/shader_final.cache")
  end

  def test_normal_dll_is_static
    assert_equal :static, Cyberpunk2077.cleanup_action("bin/x64/plugin.dll")
  end
end
