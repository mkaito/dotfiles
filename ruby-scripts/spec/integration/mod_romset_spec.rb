# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require "open3"
require "toml-rb"

MOD_BIN      = File.expand_path("../../bin/mod", __dir__) unless defined?(MOD_BIN)
GEMFILE_PATH = File.expand_path("../../Gemfile", __dir__)  unless defined?(GEMFILE_PATH)

class ModRomsetIntegrationTest < Minitest::Test
  def setup
    @dir        = Dir.mktmpdir
    @xdg_config = File.join(@dir, "config")
    @xdg_data   = File.join(@dir, "data")
    @archive_dir = File.join(@dir, "archive")
    @game_dir    = File.join(@dir, "game")

    FileUtils.mkdir_p([
      File.join(@xdg_config, "mods", "collections"),
      File.join(@xdg_config, "mods", "modsets"),
      @game_dir,
    ])

    File.write(File.join(@xdg_config, "mods", "config.toml"), TomlRB.dump(
      "archive_dir" => @archive_dir,
      "cp2077"      => { "game_dir" => @game_dir, "domain" => "cyberpunk2077" }
    ))

    make_collection("frameworks")
    make_collection("ui-mods")
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_romset_new_creates_file
    status, out = run_mod("romset", "new", "full")
    assert_equal 0, status, out
    path = rs_path("full")
    assert File.exist?(path)
    data = TomlRB.load_file(path)
    assert_equal "cyberpunk2077", data["game"]
    assert_equal [], data["collections"]
  end

  def test_romset_new_errors_if_exists
    run_mod("romset", "new", "full")
    status, out = run_mod("romset", "new", "full")
    assert_equal 1, status
    assert_match(/already exists/, out)
  end

  def test_romset_list_empty
    status, out = run_mod("romset", "list")
    assert_equal 0, status, out
    assert_match(/no romsets/, out)
  end

  def test_romset_list_shows_name_and_count
    run_mod("romset", "new", "full")
    run_mod("romset", "add", "full", "frameworks")
    status, out = run_mod("romset", "list")
    assert_equal 0, status, out
    assert_match(/full.*1 collection/, out)
  end

  def test_romset_show_empty
    run_mod("romset", "new", "full")
    status, out = run_mod("romset", "show", "full")
    assert_equal 0, status, out
    assert_match(/empty/, out)
  end

  def test_romset_show_lists_collections
    run_mod("romset", "new", "full")
    run_mod("romset", "add", "full", "frameworks", "ui-mods")
    status, out = run_mod("romset", "show", "full")
    assert_equal 0, status, out
    assert_match(/frameworks/, out)
    assert_match(/ui-mods/, out)
  end

  def test_romset_add_appends_collection
    run_mod("romset", "new", "full")
    status, out = run_mod("romset", "add", "full", "frameworks")
    assert_equal 0, status, out
    assert_equal ["frameworks"], TomlRB.load_file(rs_path("full"))["collections"]
  end

  def test_romset_add_multiple_collections
    run_mod("romset", "new", "full")
    status, out = run_mod("romset", "add", "full", "frameworks", "ui-mods")
    assert_equal 0, status, out
    assert_equal ["frameworks", "ui-mods"], TomlRB.load_file(rs_path("full"))["collections"]
  end

  def test_romset_add_idempotent
    run_mod("romset", "new", "full")
    run_mod("romset", "add", "full", "frameworks")
    run_mod("romset", "add", "full", "frameworks")
    assert_equal ["frameworks"], TomlRB.load_file(rs_path("full"))["collections"]
  end

  def test_romset_add_errors_on_unknown_collection
    run_mod("romset", "new", "full")
    status, out = run_mod("romset", "add", "full", "no-such-collection")
    assert_equal 1, status
    assert_match(/collection not found/, out)
  end

  def test_romset_remove_drops_collection
    run_mod("romset", "new", "full")
    run_mod("romset", "add", "full", "frameworks", "ui-mods")
    status, out = run_mod("romset", "remove", "full", "frameworks")
    assert_equal 0, status, out
    assert_equal ["ui-mods"], TomlRB.load_file(rs_path("full"))["collections"]
  end

  def test_romset_delete_removes_file
    run_mod("romset", "new", "full")
    status, out = run_mod("romset", "delete", "-y", "full")
    assert_equal 0, status, out
    refute File.exist?(rs_path("full"))
  end

  def test_romset_delete_errors_when_not_found
    status, out = run_mod("romset", "delete", "-y", "nonexistent")
    assert_equal 1, status
    assert_match(/not found/, out)
  end

  def test_full_flow
    run_mod("romset", "new", "my-setup")
    run_mod("romset", "add", "my-setup", "frameworks", "ui-mods")

    _, out = run_mod("romset", "show", "my-setup")
    assert_match(/frameworks/, out)
    assert_match(/ui-mods/, out)

    _, out = run_mod("romset", "list")
    assert_match(/my-setup.*2 collection/, out)

    run_mod("romset", "remove", "my-setup", "frameworks")
    _, out = run_mod("romset", "show", "my-setup")
    refute_match(/frameworks/, out)
    assert_match(/ui-mods/, out)

    run_mod("romset", "delete", "-y", "my-setup")
    refute File.exist?(rs_path("my-setup"))
  end

  private

  def run_mod(*args)
    env = {
      "XDG_CONFIG_HOME" => @xdg_config,
      "XDG_DATA_HOME"   => @xdg_data,
      "BUNDLE_GEMFILE"  => GEMFILE_PATH,
    }
    out, status = Open3.capture2e(env, "ruby", MOD_BIN, *args)
    [status.exitstatus, out]
  end

  def rs_path(name)
    File.join(@xdg_config, "mods", "modsets", "#{name}.toml")
  end

  def make_collection(name)
    path = File.join(@xdg_config, "mods", "collections", "#{name}.toml")
    File.write(path, TomlRB.dump("name" => name, "mods" => []))
  end
end
