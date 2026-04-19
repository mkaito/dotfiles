# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/adapters/deploy/redirect_store"

include ModManager::Adapters::Deploy

class CyberpunkRedirectsTest < Minitest::Test
  def filenames_for(dst_rel)
    RedirectStore::CyberpunkRedirects.filenames_for(dst_rel)
  end

  def test_non_cet_dir_returns_sqlite_only
    assert_equal %w[db.sqlite3], filenames_for("mods/nativeSettings")
  end

  def test_cet_as_path_component_includes_config_files
    result = filenames_for("bin/x64/plugins/cyber_engine_tweaks")
    assert_includes result, "db.sqlite3"
    assert_includes result, "bindings.json"
    assert_includes result, "config.json"
    assert_includes result, "layout.ini"
    assert_includes result, "persistent.json"
  end

  def test_cet_substring_in_component_name_does_not_match
    assert_equal %w[db.sqlite3], filenames_for("mods/my_cyber_engine_tweaks_mod")
  end

  def test_cet_as_non_final_component_matches
    result = filenames_for("bin/x64/plugins/cyber_engine_tweaks/scripts")
    assert_includes result, "bindings.json"
  end
end
