# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/services/game_profile/cyberpunk2077"

include ModManager::Services::GameProfile

class Cyberpunk2077CleanupActionTest < Minitest::Test
  # :migrate cases
  def test_sqlite3_anywhere_migrates
    assert_equal :migrate, Cyberpunk2077.cleanup_action("some/dir/db.sqlite3")
  end

  def test_bindings_json_in_cet_migrates
    assert_equal :migrate, Cyberpunk2077.cleanup_action("bin/x64/plugins/cyber_engine_tweaks/bindings.json")
  end

  def test_config_json_in_cet_migrates
    assert_equal :migrate, Cyberpunk2077.cleanup_action("bin/x64/plugins/cyber_engine_tweaks/config.json")
  end

  def test_layout_ini_in_cet_migrates
    assert_equal :migrate, Cyberpunk2077.cleanup_action("bin/x64/plugins/cyber_engine_tweaks/layout.ini")
  end

  def test_persistent_json_in_cet_migrates
    assert_equal :migrate, Cyberpunk2077.cleanup_action("bin/x64/plugins/cyber_engine_tweaks/persistent.json")
  end

  # :delete cases
  def test_log_extension_deletes
    assert_equal :delete, Cyberpunk2077.cleanup_action("bin/x64/plugin.log")
  end

  def test_log_variant_deletes
    assert_equal :delete, Cyberpunk2077.cleanup_action("bin/x64/Plugin.log?")
  end

  def test_vkd3d_proton_deletes
    assert_equal :delete, Cyberpunk2077.cleanup_action("bin/x64/vkd3d-proton.cache.write")
  end

  def test_final_redscripts_modded_deletes
    assert_equal :delete, Cyberpunk2077.cleanup_action("r6/cache/final.redscripts.modded")
  end

  def test_final_redscripts_ts_deletes
    assert_equal :delete, Cyberpunk2077.cleanup_action("r6/cache/final.redscripts.ts")
  end

  # :keep cases
  def test_bindings_json_outside_cet_keeps
    assert_equal :keep, Cyberpunk2077.cleanup_action("mods/bindings.json")
  end

  def test_shader_final_cache_keeps
    assert_equal :keep, Cyberpunk2077.cleanup_action("bin/x64/shader_final.cache")
  end

  def test_normal_dll_keeps
    assert_equal :keep, Cyberpunk2077.cleanup_action("bin/x64/plugin.dll")
  end
end

class Cyberpunk2077RedirectFilenamesTest < Minitest::Test
  def test_non_cet_dir_returns_sqlite_only
    assert_equal %w[db.sqlite3], Cyberpunk2077.redirect_filenames_for("mods/nativeSettings")
  end

  def test_cet_as_path_component_returns_sqlite_and_cet_configs
    result = Cyberpunk2077.redirect_filenames_for("bin/x64/plugins/cyber_engine_tweaks")
    assert_includes result, "db.sqlite3"
    assert_includes result, "bindings.json"
    assert_includes result, "config.json"
    assert_includes result, "layout.ini"
    assert_includes result, "persistent.json"
  end

  def test_cet_substring_only_does_not_match
    # "my_cyber_engine_tweaks_mod" must not trigger CET config redirect
    result = Cyberpunk2077.redirect_filenames_for("mods/my_cyber_engine_tweaks_mod")
    assert_equal %w[db.sqlite3], result
  end
end
