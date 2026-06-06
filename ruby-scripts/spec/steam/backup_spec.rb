# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "json"
require "steam/backup"
require "core/errors"

class SteamBackupTest < Minitest::Test
  def test_save_then_load_round_trips
    Dir.mktmpdir do |dir|
      path = File.join(dir, "launch-options.json")
      map = {"100" => "mangohud %command%", "300" => 'bash -c \\"x\\"'}
      Steam::Backup.save(path, map)
      assert_equal map, Steam::Backup.load(path)
    end
  end

  def test_save_sorts_keys_and_ends_with_newline
    Dir.mktmpdir do |dir|
      path = File.join(dir, "launch-options.json")
      Steam::Backup.save(path, {"300" => "c", "100" => "a", "200" => "b"})
      body = File.read(path)
      assert_equal ["100", "200", "300"], JSON.parse(body).keys
      assert body.end_with?("\n"), "file should end with a trailing newline"
    end
  end

  def test_load_missing_file_raises_core_error
    assert_raises(Core::Error) { Steam::Backup.load("/nonexistent/launch-options.json") }
  end
end
