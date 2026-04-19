# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/adapters/runtime_launch/fuse_overlayfs"

include ModManager

class FuseOverlayfsTest < Minitest::Test
  def setup
    @game_dir = "/games/Cyberpunk 2077"
    @data_dir = "/data/mod-data"
    @adapter = Adapters::RuntimeLaunch::FuseOverlayfs.new(game_dir: @game_dir, data_dir: @data_dir)
  end

  def test_raises_when_fuse_overlayfs_missing
    adapter = Adapters::RuntimeLaunch::FuseOverlayfs.new(game_dir: "/nonexistent", data_dir: @data_dir)
    stub_path("") do
      assert_raises(Error) { adapter.launch(mods: [], modset: "main", command: ["echo"]) }
    end
  end

  def test_raises_when_game_dir_missing
    with_fake_bins do |bin_dir|
      stub_path(bin_dir) do
        adapter = Adapters::RuntimeLaunch::FuseOverlayfs.new(game_dir: "/does/not/exist", data_dir: @data_dir)
        e = assert_raises(Error) { adapter.launch(mods: [], modset: "main", command: ["echo"]) }
        assert_match(/game_dir not found/, e.message)
      end
    end
  end

  def test_script_has_fuse_overlayfs_mount_bind_run_cleanup_in_order
    script = script_for(mods: [], game_dir: "/game", data_dir: "/data")
    lines = script.lines.map(&:strip).reject(&:empty?)
    fuse_idx     = lines.index { _1.start_with?("fuse-overlayfs") }
    bind_idx     = lines.index { _1.start_with?("mount --bind") }
    run_idx      = lines.index { _1 == '"$@"' }
    fusermount_idx = lines.index { _1.start_with?("fusermount -u") }

    refute_nil fuse_idx,       "fuse-overlayfs line missing"
    refute_nil bind_idx,       "mount --bind line missing"
    refute_nil run_idx,        '"$@" line missing'
    refute_nil fusermount_idx, "fusermount -u line missing"

    assert fuse_idx < bind_idx,       "fuse-overlayfs must come before mount --bind"
    assert bind_idx < run_idx,        "mount --bind must come before command"
    assert run_idx  < fusermount_idx, "command must come before fusermount cleanup"
  end

  def test_lowerdir_contains_mod_paths_then_game_dir
    mod_a = Struct.new(:path).new("/archive/mod-a")
    mod_b = Struct.new(:path).new("/archive/mod-b")
    script = script_for(mods: [mod_a, mod_b], game_dir: "/game", data_dir: "/data")
    fuse_line = script.lines.find { _1.start_with?("fuse-overlayfs") }
    assert_includes fuse_line, "/archive/mod-a:/archive/mod-b:/game"
  end

  def test_spaces_in_paths_are_shell_escaped_in_script
    mod = Struct.new(:path).new("/archive/my mod")
    script = script_for(mods: [mod], game_dir: "/games/Cyberpunk 2077", data_dir: "/data")
    # Shellwords.shellescape uses backslash escaping for spaces
    assert_includes script, "/games/Cyberpunk\\ 2077"
    assert_includes script, "/archive/my\\ mod"
  end

  private

  def script_for(mods:, game_dir:, data_dir:)
    adapter = Adapters::RuntimeLaunch::FuseOverlayfs.new(game_dir:, data_dir:)
    lowerdir = (mods.map(&:path) + [game_dir]).join(":")
    options  = "lowerdir=#{lowerdir},upperdir=#{data_dir}/main/upper,workdir=#{data_dir}/main/work"
    adapter.send(:build_script, options, "#{data_dir}/main/merged", game_dir)
  end

  def stub_path(path_value)
    old = ENV["PATH"]
    ENV["PATH"] = path_value
    yield
  ensure
    ENV["PATH"] = old
  end

  def with_fake_bins
    require "tmpdir"
    Dir.mktmpdir do |dir|
      %w[fuse-overlayfs fusermount unshare].each do |bin|
        path = File.join(dir, bin)
        File.write(path, "#!/bin/sh\n")
        File.chmod(0o755, path)
      end
      yield dir
    end
  end
end
