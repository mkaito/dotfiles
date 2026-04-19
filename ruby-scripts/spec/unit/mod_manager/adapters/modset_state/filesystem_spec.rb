# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require "mod_manager/adapters/modset_state/filesystem"

include ModManager

class ModsetStateFilesystemTest < Minitest::Test
  def setup
    @dir  = Dir.mktmpdir
    @adapter = Adapters::ModsetState::Filesystem.new(data_dir: @dir)
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_summary_returns_nil_when_no_state_dir
    assert_nil @adapter.summary(modset: "main")
  end

  def test_summary_returns_zero_bytes_for_empty_dir
    FileUtils.mkdir_p(File.join(@dir, "main"))
    s = @adapter.summary(modset: "main")
    assert_equal 0, s[:redirect_bytes]
    assert_equal 0, s[:overlay_bytes]
  end

  def test_summary_splits_redirect_and_overlay
    write_file("main/mods/native/db.sqlite3", "x" * 100)
    write_file("main/upper/bin/x64/config.json", "y" * 200)

    s = @adapter.summary(modset: "main")
    assert_equal 100, s[:redirect_bytes]
    assert_equal 200, s[:overlay_bytes]
  end

  def test_summary_ignores_overlayfs_whiteout_files
    write_file("main/upper/r6/logs/.wh..wh..opq", "")          # opaque dir marker
    write_file("main/upper/r6/logs/redscript.log", "x" * 100)  # real file
    write_file("main/upper/red4ext/logs/.wh.old.log", "")      # individual whiteout

    s = @adapter.summary(modset: "main")
    assert_equal 100, s[:overlay_bytes]
  end

  def test_details_excludes_whiteout_files
    write_file("main/upper/r6/logs/.wh..wh..opq", "")
    write_file("main/upper/r6/logs/redscript.log", "x" * 42)

    d = @adapter.details(modset: "main")
    paths = d[:overlay].map { _1[:path] }
    assert_equal %w[r6/logs/redscript.log], paths
  end

  def test_summary_ignores_work_and_merged_dirs
    write_file("main/mods/native/db.sqlite3", "x" * 50)
    write_file("main/work/internal", "w" * 999)
    write_file("main/merged/should_ignore", "m" * 999)

    s = @adapter.summary(modset: "main")
    assert_equal 50, s[:redirect_bytes]
    assert_equal 0,  s[:overlay_bytes]
  end

  def test_details_returns_sorted_paths_relative_to_base
    write_file("main/mods/b/db.sqlite3", "bb")
    write_file("main/mods/a/db.sqlite3", "a")
    write_file("main/upper/config.json", "cfg")

    d = @adapter.details(modset: "main")
    assert_equal %w[mods/a/db.sqlite3 mods/b/db.sqlite3], d[:redirect].map { _1[:path] }
    assert_equal %w[config.json], d[:overlay].map { _1[:path] }
    assert_equal 1, d[:redirect].find { _1[:path] == "mods/a/db.sqlite3" }[:bytes]
    assert_equal 2, d[:redirect].find { _1[:path] == "mods/b/db.sqlite3" }[:bytes]
  end

  def test_wipe_removes_dir_and_returns_bytes
    write_file("main/mods/native/db.sqlite3", "x" * 100)
    write_file("main/upper/config.json", "y" * 50)

    result = @adapter.wipe(modset: "main")
    assert_equal 150, result[:bytes_freed]
    refute File.exist?(File.join(@dir, "main"))
  end

  def test_wipe_returns_zero_for_missing_dir
    result = @adapter.wipe(modset: "nonexistent")
    assert_equal 0, result[:bytes_freed]
  end

  private

  def write_file(rel, content)
    path = File.join(@dir, rel)
    FileUtils.mkdir_p(File.dirname(path))
    File.write(path, content)
  end
end
