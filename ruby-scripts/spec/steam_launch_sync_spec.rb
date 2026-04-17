# frozen_string_literal: true
require "minitest/autorun"
require "toml-rb"
require "vdf"
require_relative "../lib/steam/vdf_nav"

SAMPLE_VDF = <<~VDF
  "UserLocalConfigStore"
  {
    "Software"
    {
      "Valve"
      {
        "Steam"
        {
          "apps"
          {
            "294100"
            {
              "LaunchOptions"    "mangohud %command%"
              "LastPlayed"       "1712000000"
              "Playtime"         "4242"
              "cloudstorage"     "1"
            }
            "730"
            {
              "LastPlayed"       "1711000000"
              "Playtime"         "9999"
            }
          }
        }
      }
    }
  }
VDF

def apps_hash(vdf_data)
  Steam::VdfNav.dig(vdf_data, "UserLocalConfigStore", "Software", "Valve", "Steam", "apps")
end

class VdfRoundTripTest < Minitest::Test
  def test_parse_generate_parse_produces_identical_data
    original = VDF.parse(SAMPLE_VDF)
    assert_equal original, VDF.parse(VDF.generate(original))
  end

  def test_all_apps_survive_round_trip
    original = VDF.parse(SAMPLE_VDF)
    reparsed = VDF.parse(VDF.generate(original))
    assert_equal apps_hash(original).keys.sort, apps_hash(reparsed).keys.sort
  end

  def test_values_with_double_quotes_survive_round_trip
    vdf_src = %("apps"\n{\n  "1"\n  {\n    "LaunchOptions"    "bash -c \\"foo bar\\" %command%"\n  }\n}\n)
    reparsed = VDF.parse(VDF.generate(VDF.parse(vdf_src)))
    # vdf gem escapes quotes as \" on generate; re-parse keeps the backslash
    assert_equal 'bash -c \\"foo bar\\" %command%', Steam::VdfNav.dig(reparsed, "apps", "1", "LaunchOptions")
  end
end

class ExportPreservationTest < Minitest::Test
  def setup
    @vdf_data = VDF.parse(SAMPLE_VDF)
  end

  def test_export_updates_targeted_key
    Steam::VdfNav.set(@vdf_data, "UserLocalConfigStore", "Software", "Valve", "Steam",
               "apps", "294100", "LaunchOptions", value: "gamemoderun %command%")
    reparsed = VDF.parse(VDF.generate(@vdf_data))
    assert_equal "gamemoderun %command%",
      Steam::VdfNav.dig(reparsed, "UserLocalConfigStore", "Software", "Valve", "Steam", "apps", "294100", "LaunchOptions")
  end

  def test_export_preserves_untracked_keys_in_same_app
    Steam::VdfNav.set(@vdf_data, "UserLocalConfigStore", "Software", "Valve", "Steam",
               "apps", "294100", "LaunchOptions", value: "new value")
    reparsed = VDF.parse(VDF.generate(@vdf_data))
    assert_equal 1712000000, Steam::VdfNav.dig(reparsed, "UserLocalConfigStore", "Software", "Valve", "Steam", "apps", "294100", "LastPlayed")
    assert_equal 4242,         Steam::VdfNav.dig(reparsed, "UserLocalConfigStore", "Software", "Valve", "Steam", "apps", "294100", "Playtime")
    assert_equal 1,            Steam::VdfNav.dig(reparsed, "UserLocalConfigStore", "Software", "Valve", "Steam", "apps", "294100", "cloudstorage")
  end

  def test_export_preserves_untargeted_apps_entirely
    Steam::VdfNav.set(@vdf_data, "UserLocalConfigStore", "Software", "Valve", "Steam",
               "apps", "294100", "LaunchOptions", value: "new value")
    reparsed = VDF.parse(VDF.generate(@vdf_data))
    assert_equal({ "LastPlayed" => 1711000000, "Playtime" => 9999 },
      Steam::VdfNav.dig(reparsed, "UserLocalConfigStore", "Software", "Valve", "Steam", "apps", "730"))
  end
end

class ExportSkipUnknownAppTest < Minitest::Test
  def test_does_not_create_new_app_sections
    vdf_data = VDF.parse(SAMPLE_VDF)
    apps = apps_hash(vdf_data)
    toml = { "99999" => { "options" => "mangohud %command%" } }
    toml.each do |appid, section|
      next unless section.is_a?(Hash)
      existing_key = apps.keys.find { |k| k.casecmp?(appid) }
      next unless existing_key
      Steam::VdfNav.set(vdf_data, "UserLocalConfigStore", "Software", "Valve", "Steam",
                 "apps", existing_key, "LaunchOptions", value: section["options"])
    end
    reparsed = VDF.parse(VDF.generate(vdf_data))
    new_apps = apps_hash(reparsed)
    refute new_apps.any? { |k, _| k == "99999" }
    assert_equal apps.size, new_apps.size
  end
end

class ImportTest < Minitest::Test
  def test_skips_empty_launch_options
    apps = apps_hash(VDF.parse(SAMPLE_VDF))
    toml = {}
    apps.each do |appid, data|
      next unless data.is_a?(Hash)
      _, val = data.find { |k, _| k.casecmp?("LaunchOptions") }
      next if val.nil? || val.empty?
      toml[appid] = { "options" => val }
    end
    assert_equal ["294100"], toml.keys
    assert_equal "mangohud %command%", toml["294100"]["options"]
  end

  def test_import_preserves_existing_name_key
    existing_toml = { "294100" => { "name" => "Barotrauma", "options" => "old" } }
    apps = apps_hash(VDF.parse(SAMPLE_VDF))
    apps.each do |appid, data|
      next unless data.is_a?(Hash)
      _, val = data.find { |k, _| k.casecmp?("LaunchOptions") }
      next if val.nil? || val.empty?
      existing_toml[appid] ||= {}
      existing_toml[appid]["options"] = val
    end
    assert_equal "Barotrauma", existing_toml["294100"]["name"]
    assert_equal "mangohud %command%", existing_toml["294100"]["options"]
  end
end
