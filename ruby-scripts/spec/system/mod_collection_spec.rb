# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require "toml-rb"
require "support/mod_cli_helper"

class ModCollectionIntegrationTest < Minitest::Test
  include ModCliHelper

  def setup
    @dir = Dir.mktmpdir
    @xdg_config = File.join(@dir, "config")
    @xdg_data = File.join(@dir, "data")
    @archive_dir = File.join(@dir, "archive")
    @game_dir = File.join(@dir, "game")

    FileUtils.mkdir_p([
      File.join(@xdg_config, "mods", "collections"),
      File.join(@xdg_config, "mods", "modsets"),
      @game_dir
    ])

    File.write(File.join(@xdg_config, "mods", "config.toml"), TomlRB.dump(
      "archive_dir" => @archive_dir,
      "cp2077" => {"game_dir" => @game_dir, "domain" => "cyberpunk2077"}
    ))

    make_mod("mod-a")
    make_mod("mod-b")
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_collection_new_creates_file
    status, out = run_mod("collection", "new", "frameworks")
    assert_equal 0, status, out
    path = col_path("frameworks")
    assert File.exist?(path)
    data = TomlRB.load_file(path)
    assert_equal "frameworks", data["name"]
    assert_equal [], data["mods"]
  end

  def test_collection_new_errors_if_exists
    run_mod("collection", "new", "frameworks")
    status, out = run_mod("collection", "new", "frameworks")
    assert_equal 1, status
    assert_match(/already exists/, out)
  end

  def test_collection_list_empty
    status, out = run_mod("collection", "list")
    assert_equal 0, status, out
    assert_match(/no collections/, out)
  end

  def test_collection_list_shows_name_and_count
    run_mod("collection", "new", "frameworks")
    run_mod("collection", "add", "frameworks", "mod-a")
    status, out = run_mod("collection", "list")
    assert_equal 0, status, out
    assert_match(/frameworks: 1 mod/, out)
  end

  def test_collection_show_empty
    run_mod("collection", "new", "frameworks")
    status, out = run_mod("collection", "show", "frameworks")
    assert_equal 0, status, out
    assert_match(/empty/, out)
  end

  def test_collection_show_lists_mods
    run_mod("collection", "new", "frameworks")
    run_mod("collection", "add", "frameworks", "mod-a")
    run_mod("collection", "add", "frameworks", "mod-b")
    status, out = run_mod("collection", "show", "frameworks")
    assert_equal 0, status, out
    assert_match(/mod-a/, out)
    assert_match(/mod-b/, out)
  end

  def test_collection_add_appends_slug
    run_mod("collection", "new", "frameworks")
    status, out = run_mod("collection", "add", "frameworks", "mod-a")
    assert_equal 0, status, out
    assert_equal ["mod-a"], TomlRB.load_file(col_path("frameworks"))["mods"]
  end

  def test_collection_add_errors_on_unknown_slug
    run_mod("collection", "new", "frameworks")
    status, out = run_mod("collection", "add", "frameworks", "not-in-archive")
    assert_equal 1, status
    assert_match(/not in archive/, out)
  end

  def test_collection_add_idempotent
    run_mod("collection", "new", "frameworks")
    run_mod("collection", "add", "frameworks", "mod-a")
    run_mod("collection", "add", "frameworks", "mod-a")
    assert_equal ["mod-a"], TomlRB.load_file(col_path("frameworks"))["mods"]
  end

  def test_collection_remove_drops_slug
    run_mod("collection", "new", "frameworks")
    run_mod("collection", "add", "frameworks", "mod-a")
    run_mod("collection", "add", "frameworks", "mod-b")
    status, out = run_mod("collection", "remove", "frameworks", "mod-a")
    assert_equal 0, status, out
    assert_equal ["mod-b"], TomlRB.load_file(col_path("frameworks"))["mods"]
  end

  def test_collection_delete_removes_file
    run_mod("collection", "new", "frameworks")
    status, out = run_mod("collection", "delete", "-y", "frameworks")
    assert_equal 0, status, out
    refute File.exist?(col_path("frameworks"))
  end

  def test_collection_delete_errors_when_not_found
    status, out = run_mod("collection", "delete", "-y", "nonexistent")
    assert_equal 1, status
    assert_match(/not found/, out)
  end

  def test_full_flow
    run_mod("collection", "new", "my-mods")

    run_mod("collection", "add", "my-mods", "mod-a")
    run_mod("collection", "add", "my-mods", "mod-b")

    _, out = run_mod("collection", "show", "my-mods")
    assert_match(/mod-a/, out)
    assert_match(/mod-b/, out)

    _, out = run_mod("collection", "list")
    assert_match(/my-mods: 2 mod/, out)

    run_mod("collection", "remove", "my-mods", "mod-a")

    _, out = run_mod("collection", "show", "my-mods")
    refute_match(/mod-a/, out)
    assert_match(/mod-b/, out)

    run_mod("collection", "delete", "-y", "my-mods")
    refute File.exist?(col_path("my-mods"))
  end

  private

  def col_path(name)
    File.join(@xdg_config, "mods", "collections", "#{name}.toml")
  end

  def make_mod(slug)
    dir = File.join(@archive_dir, "cyberpunk2077", slug)
    FileUtils.mkdir_p(dir)
    File.write(File.join(dir, "meta.toml"), TomlRB.dump(
      "name" => slug,
      "slug" => slug,
      "version" => "1.0",
      "game" => "cyberpunk2077",
      "depends" => []
    ))
  end
end
