# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require "toml-rb"
require "support/mod_cli_helper"

class LaunchGameSystemTest < Minitest::Test
  include ModCliHelper

  def setup
    @dir = Dir.mktmpdir
    @xdg_config = File.join(@dir, "config")
    @xdg_data   = File.join(@dir, "data")
    @archive_dir = File.join(@dir, "archive")
    @game_dir    = File.join(@dir, "game")

    FileUtils.mkdir_p([
      File.join(@xdg_config, "mods", "collections"),
      File.join(@xdg_config, "mods", "modsets"),
      @game_dir
    ])

    File.write(File.join(@xdg_config, "mods", "config.toml"), TomlRB.dump(
      "archive_dir" => @archive_dir,
      "cp2077" => {"game_dir" => @game_dir, "domain" => "cyberpunk2077"}
    ))

    # Base game file — must remain visible through overlay
    File.write(File.join(@game_dir, "base_file.txt"), "base")

    # Mod with a file that should appear at game_dir/mod_file.txt
    make_mod("mymod", files: {"mod_file.txt" => "mod content"})
    make_collection("core", %w[mymod])
    make_modset("main", collections: %w[core])
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_overlay_merges_mod_files_into_game_dir
    fake_command = [
      "sh", "-c",
      "test -f #{@game_dir}/mod_file.txt && test -f #{@game_dir}/base_file.txt"
    ]
    status, out = run_mod("launch", "--modset=main", *fake_command)
    assert_equal 0, status, "overlay did not merge mod file into game_dir\n#{out}"
  end

  def test_cow_write_lands_in_upperdir
    # data_dir = File.dirname(archive_dir) / "mod-data" — archive_dir is @dir/archive, so dirname is @dir
    upper_file = File.join(@dir, "mod-data", "main", "upper", "mod_file.txt")
    fake_command = [
      "sh", "-c",
      "echo 'state' > #{@game_dir}/mod_file.txt"
    ]
    status, out = run_mod("launch", "--modset=main", *fake_command)
    assert_equal 0, status, out
    assert File.exist?(upper_file), "COW write did not land in upperdir"
    assert_equal "state\n", File.read(upper_file)
  end

  def test_game_dir_has_no_lingering_bind_mount_after_exit
    fake_command = ["sh", "-c", "true"]
    run_mod("launch", "--modset=main", *fake_command)

    # After exit, game_dir should not have mod_file.txt (it's not a real game file)
    refute File.exist?(File.join(@game_dir, "mod_file.txt")),
      "mod_file.txt still visible in game_dir after launch — bind mount not cleaned up"
  end

  def test_failed_command_returns_nonzero_exit_code
    fake_command = ["sh", "-c", "exit 42"]
    status, = run_mod("launch", "--modset=main", *fake_command)
    assert_equal 42, status
  end

  # Simulates: AMD_USERQ=1 WINEDLLOVERRIDES="..." mod launch --modset=main -- %command% --skipStartScreen --modded --launcher-skip
  # -- is the explicit terminator; OptionParser removes it and stops, leaving %command% expansion intact.
  def test_double_dash_separator_passes_command_through
    wrapper = File.join(@dir, "proton_wrapper.sh")
    arg_dump = File.join(@dir, "args.txt")
    File.write(wrapper, <<~SH)
      #!/bin/sh
      printf '%s\\n' "$@" > #{arg_dump}
      exit 0
    SH
    File.chmod(0o755, wrapper)

    status, out = run_mod(
      "launch", "--modset=main", "--",
      wrapper, "--launcher-skip", "-skipStartScreen", "-modded"
    )
    assert_equal 0, status, "launch failed\n#{out}"
    assert File.exist?(arg_dump), "wrapper was never called"
    args = File.read(arg_dump).split("\n")
    assert_equal %w[--launcher-skip -skipStartScreen -modded], args
  end

  # Simulates: AMD_USERQ=1 WINEDLLOVERRIDES="..." mod launch --modset=main %command% --skipStartScreen --modded --launcher-skip
  # Verifies that order! stops at first non-option so game args are not consumed by OptionParser.
  def test_steam_game_args_passed_through_untouched
    # Wrapper script acts as the Proton/Steam Runtime stand-in; records argv to a file
    wrapper = File.join(@dir, "proton_wrapper.sh")
    arg_dump = File.join(@dir, "args.txt")
    File.write(wrapper, <<~SH)
      #!/bin/sh
      printf '%s\\n' "$@" > #{arg_dump}
      exit 0
    SH
    File.chmod(0o755, wrapper)

    status, out = run_mod(
      "launch", "--modset=main",
      wrapper, "--skipStartScreen", "--modded", "--launcher-skip"
    )
    assert_equal 0, status, "launch failed\n#{out}"
    assert File.exist?(arg_dump), "wrapper was never called"
    args = File.read(arg_dump).split("\n")
    assert_equal %w[--skipStartScreen --modded --launcher-skip], args
  end

  def test_multiword_command_all_forwarded
    # Simulates Proton wrapper expanding to multiple words before the game binary.
    # e.g. pressure-vessel-wrap -- wine64 CyberpunkGame.exe
    receiver = File.join(@dir, "receiver.sh")
    arg_dump = File.join(@dir, "args.txt")
    File.write(receiver, <<~SH)
      #!/bin/sh
      printf '%s\\n' "$@" > #{arg_dump}
      exit 0
    SH
    File.chmod(0o755, receiver)

    status, out = run_mod(
      "launch", "--modset=main",
      receiver, "wine64", "CyberpunkGame.exe", "--skipStartScreen"
    )
    assert_equal 0, status, "launch failed\n#{out}"
    assert File.exist?(arg_dump), "receiver was never called"
    args = File.read(arg_dump).split("\n")
    assert_equal %w[wine64 CyberpunkGame.exe --skipStartScreen], args
  end

  private

  def make_mod(slug, files: {})
    dir = File.join(@archive_dir, "cyberpunk2077", slug)
    files.each do |rel, content|
      path = File.join(dir, rel)
      FileUtils.mkdir_p(File.dirname(path))
      File.write(path, content)
    end
    File.write(File.join(dir, "meta.toml"), TomlRB.dump(
      "name" => slug, "slug" => slug, "version" => "1.0",
      "game" => "cyberpunk2077", "depends" => [], "source" => {}
    ))
  end

  def make_collection(name, slugs)
    File.write(
      File.join(@xdg_config, "mods", "collections", "#{name}.toml"),
      TomlRB.dump("name" => name, "mods" => slugs)
    )
  end

  def make_modset(name, collections:)
    File.write(
      File.join(@xdg_config, "mods", "modsets", "#{name}.toml"),
      TomlRB.dump("game" => "cyberpunk2077", "collections" => collections, "mods" => [])
    )
  end
end
