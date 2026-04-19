# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/services/game_profile/cyberpunk2077"

include ModManager::Services::GameProfile

class Cyberpunk2077RedirectsTest < Minitest::Test
  def filenames_for(dst_rel)
    Cyberpunk2077::Redirects.filenames_for(dst_rel)
  end

  def test_returns_sqlite_for_any_dir
    assert_equal %w[db.sqlite3], filenames_for("mods/nativeSettings")
  end

  def test_returns_sqlite_for_cet_dir
    assert_equal %w[db.sqlite3], filenames_for("bin/x64/plugins/cyber_engine_tweaks")
  end
end
