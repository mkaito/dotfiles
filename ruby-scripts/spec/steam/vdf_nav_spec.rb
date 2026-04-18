# frozen_string_literal: true
require "minitest/autorun"
require "steam/vdf_nav"

class SteamVdfNavDigTest < Minitest::Test
  DATA = {
    "UserLocalConfigStore" => {
      "Software" => { "Valve" => { "Steam" => { "apps" => {
        "294100" => { "LaunchOptions" => "mangohud %command%" }
      } } } }
    }
  }.freeze

  def test_exact_path
    result = Steam::VdfNav.dig(DATA, "UserLocalConfigStore", "Software", "Valve", "Steam", "apps")
    assert_equal({ "294100" => { "LaunchOptions" => "mangohud %command%" } }, result)
  end

  def test_case_insensitive_keys
    refute_nil Steam::VdfNav.dig(DATA, "userLocalConfigStore", "software", "valve", "steam", "APPS")
  end

  def test_missing_key_returns_nil
    assert_nil Steam::VdfNav.dig(DATA, "UserLocalConfigStore", "nonexistent")
  end

  def test_non_hash_mid_path_returns_nil
    assert_nil Steam::VdfNav.dig(DATA, "UserLocalConfigStore", "Software", "Valve", "Steam",
                                 "apps", "294100", "LaunchOptions", "too_deep")
  end
end

class SteamVdfNavSetTest < Minitest::Test
  def test_sets_new_leaf
    hash = { "apps" => { "123" => {} } }
    Steam::VdfNav.set(hash, "apps", "123", "LaunchOptions", value: "mangohud %command%")
    assert_equal "mangohud %command%", hash["apps"]["123"]["LaunchOptions"]
  end

  def test_preserves_existing_key_casing
    hash = { "Apps" => { "123" => { "LaunchOptions" => "old" } } }
    Steam::VdfNav.set(hash, "apps", "123", "launchoptions", value: "new")
    assert_equal "new", hash["Apps"]["123"]["LaunchOptions"]
  end

  def test_creates_intermediate_hashes
    hash = {}
    Steam::VdfNav.set(hash, "a", "b", "c", value: "v")
    assert_equal "v", hash["a"]["b"]["c"]
  end
end
