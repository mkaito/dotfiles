# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require "toml-rb"
require "support/mod_cli_helper"

class ModDeployIntegrationTest < Minitest::Test
  include ModCliHelper

  def setup
    @dir         = Dir.mktmpdir
    @xdg_config  = File.join(@dir, "config")
    @xdg_data    = File.join(@dir, "data")
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
  end

  def teardown = FileUtils.rm_rf(@dir)

  # 1. Basic deploy — files accessible, some archive symlink in path points into archive
  def test_basic_deploy_creates_symlinks
    make_mod("cet", files: { "bin/x64/global.ini" => "cet-config" })
    make_collection("frameworks", %w[cet])
    make_modset("full", collections: %w[frameworks])

    status, out = run_mod("modset", "deploy", "full")
    assert_equal 0, status, out

    assert_deployed "bin/x64/global.ini"
  end

  # 2. Multi-collection deploy — all files accessible
  def test_multi_collection_deploy
    make_mod("cet",       files: { "bin/x64/global.ini"  => "cet" })
    make_mod("redscript", files: { "r6/scripts/red.reds" => "rs" })
    make_collection("frameworks", %w[cet redscript])
    make_mod("hud",       files: { "archive/pc/mod/hud.archive" => "hud" })
    make_collection("ui", %w[hud])
    make_modset("full", collections: %w[frameworks ui])

    status, out = run_mod("modset", "deploy", "full")
    assert_equal 0, status, out

    assert_deployed "bin/x64/global.ini"
    assert_deployed "r6/scripts/red.reds"
    assert_deployed "archive/pc/mod/hud.archive"
  end

  # 3. Version conflict (same mod_id, different slugs) — last collection wins, conflict reported
  def test_version_conflict_last_collection_wins
    make_mod("cet-3.12", files: { "bin/x64/global.ini" => "old" }, mod_id: 9999)
    make_mod("cet-3.13", files: { "bin/x64/global.ini" => "new" }, mod_id: 9999)
    make_collection("old-frameworks", %w[cet-3.12])
    make_collection("new-frameworks", %w[cet-3.13])
    make_modset("full", collections: %w[old-frameworks new-frameworks])

    status, out = run_mod("modset", "deploy", "full")
    assert_equal 0, status, out
    assert_match(/conflict/, out)
    assert_match(/cet-3.12.*overridden.*cet-3.13|cet-3.13.*overrides.*cet-3.12/i, out)

    assert_deployed_from "bin/x64/global.ini", "cet-3.13"
  end

  # 4. File conflict (two different mods, same relative path) — overlay model, last mod wins
  def test_file_conflict_overlay_last_mod_wins
    make_mod("mod-a", files: { "r6/scripts/shared.reds" => "from-a" })
    make_mod("mod-b", files: { "r6/scripts/shared.reds" => "from-b" })
    make_collection("mixed", %w[mod-a mod-b])
    make_modset("full", collections: %w[mixed])

    status, out = run_mod("modset", "deploy", "full")
    assert_equal 0, status, out

    assert_deployed_from "r6/scripts/shared.reds", "mod-b"
  end

  # 5. Redeploy with different modset — old symlinks gone, new in place, real files untouched
  def test_redeploy_swaps_symlinks_preserves_real_files
    make_mod("mod-a", files: { "bin/x64/a.dll" => "a" })
    make_mod("mod-b", files: { "bin/x64/b.dll" => "b" })
    make_collection("set-a", %w[mod-a])
    make_collection("set-b", %w[mod-b])
    make_modset("rs-a", collections: %w[set-a])
    make_modset("rs-b", collections: %w[set-b])

    # Plant a real game file that must survive
    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    File.write(File.join(@game_dir, "bin/x64/native.dll"), "real game file")

    run_mod("modset", "deploy", "rs-a")
    assert_deployed "bin/x64/a.dll"

    status, out = run_mod("modset", "deploy", "rs-b")
    assert_equal 0, status, out

    refute File.exist?(File.join(@game_dir, "bin/x64/a.dll")), "old file should be gone after redeploy"
    assert_deployed "bin/x64/b.dll"
    assert File.file?(File.join(@game_dir, "bin/x64/native.dll")), "real file must survive"
  end

  # 6. Pre-flight: real file at destination — abort without touching anything
  def test_preflight_aborts_on_real_file
    make_mod("cet", files: { "bin/x64/global.ini" => "cet" })
    make_collection("frameworks", %w[cet])
    make_modset("full", collections: %w[frameworks])

    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    File.write(File.join(@game_dir, "bin/x64/global.ini"), "real game file")

    status, out = run_mod("modset", "deploy", "full")
    assert_equal 1, status
    assert_match(/cannot overwrite/, out)
    assert File.file?(File.join(@game_dir, "bin/x64/global.ini")), "real file must be intact"
  end

  # 7. Pre-flight: foreign symlink at destination — abort
  def test_preflight_aborts_on_foreign_symlink
    make_mod("cet", files: { "bin/x64/global.ini" => "cet" })
    make_collection("frameworks", %w[cet])
    make_modset("full", collections: %w[frameworks])

    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    File.symlink("/etc/hosts", File.join(@game_dir, "bin/x64/global.ini"))

    status, out = run_mod("modset", "deploy", "full")
    assert_equal 1, status
    assert_match(/cannot overwrite/, out)
  end

  # 8. Empty modset — exit 1 with clear message
  def test_empty_modset_aborts
    make_collection("empty-col", [])
    make_modset("empty", collections: %w[empty-col])

    status, out = run_mod("modset", "deploy", "empty")
    assert_equal 1, status
    assert_match(/no mods to deploy/, out)
  end

  # 9. Reset after deploy — symlinks and empty dirs removed, real files/dirs preserved
  def test_reset_removes_symlinks_and_empty_dirs
    make_mod("cet", files: { "bin/x64/global.ini" => "cet", "bin/x64/config.ini" => "cfg" })
    make_collection("frameworks", %w[cet])
    make_modset("full", collections: %w[frameworks])

    # Plant a real game file in a shared dir
    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    File.write(File.join(@game_dir, "bin/x64/native.dll"), "real")

    run_mod("modset", "deploy", "full")

    status, out = run_mod("reset", "-y")
    assert_equal 0, status, out

    # Symlinks gone
    refute File.exist?(File.join(@game_dir, "bin/x64/global.ini"))
    refute File.exist?(File.join(@game_dir, "bin/x64/config.ini"))

    # Dir kept because real file is still there
    assert File.directory?(File.join(@game_dir, "bin/x64"))
    assert File.file?(File.join(@game_dir, "bin/x64/native.dll"))
  end

  def test_reset_removes_empty_dirs
    make_mod("cet", files: { "r6/scripts/cet.reds" => "cet" })
    make_collection("frameworks", %w[cet])
    make_modset("full", collections: %w[frameworks])

    run_mod("modset", "deploy", "full")
    assert File.directory?(File.join(@game_dir, "r6/scripts"))

    run_mod("reset", "-y")
    refute File.exist?(File.join(@game_dir, "r6/scripts"))
    refute File.exist?(File.join(@game_dir, "r6"))
  end

  # 10. `mod status` reflects deployed mods
  def test_status_shows_deployed_mods
    make_mod("cet",       files: { "bin/x64/global.ini"  => "cet" })
    make_mod("redscript", files: { "r6/scripts/red.reds" => "rs" })
    make_collection("frameworks", %w[cet redscript])
    make_modset("full", collections: %w[frameworks])

    run_mod("modset", "deploy", "full")
    status, out = run_mod("status")

    assert_equal 0, status, out
    assert_match(/cet/,       out)
    assert_match(/redscript/, out)
  end

  private

  def make_mod(slug, files: {}, mod_id: nil)
    dir = File.join(@archive_dir, "cyberpunk2077", slug)
    files.each do |rel, content|
      path = File.join(dir, rel)
      FileUtils.mkdir_p(File.dirname(path))
      File.write(path, content)
    end
    source = mod_id ? { "provider" => "nexus", "mod_id" => mod_id } : {}
    File.write(File.join(dir, "meta.toml"), TomlRB.dump(
      "name"    => slug,
      "slug"    => slug,
      "version" => "1.0",
      "game"    => "cyberpunk2077",
      "depends" => [],
      "source"  => source
    ))
  end

  def make_collection(name, slugs)
    path = File.join(@xdg_config, "mods", "collections", "#{name}.toml")
    File.write(path, TomlRB.dump("name" => name, "mods" => slugs))
  end

  def make_modset(name, collections:)
    path = File.join(@xdg_config, "mods", "modsets", "#{name}.toml")
    File.write(path, TomlRB.dump("game" => "cyberpunk2077", "collections" => collections, "mods" => []))
  end

  # Assert that rel_path is accessible under game_dir via an archive symlink somewhere
  # in its ancestor chain. Works regardless of whether the symlink is at the file level
  # or a higher directory level.
  def assert_deployed(rel_path)
    full = File.join(@game_dir, rel_path)
    assert File.exist?(full), "expected #{rel_path} to be accessible after deploy"
    assert archive_symlink_in_path?(rel_path), "expected an archive symlink in path to #{rel_path}"
  end

  def assert_deployed_from(rel_path, slug_fragment)
    assert_deployed(rel_path)
    symlink = archive_symlink_for(rel_path)
    assert symlink, "no archive symlink found in path to #{rel_path}"
    target = File.readlink(symlink)
    assert_includes target, slug_fragment, "expected symlink to #{rel_path} to point to #{slug_fragment}"
  end

  def archive_symlink_in_path?(rel_path)
    !!archive_symlink_for(rel_path)
  end

  def archive_symlink_for(rel_path)
    parts = rel_path.split("/")
    parts.size.times do |i|
      candidate = File.join(@game_dir, *parts[0..i])
      if File.symlink?(candidate)
        target = File.expand_path(File.readlink(candidate), File.dirname(candidate))
        return candidate if target.start_with?(@archive_dir)
      end
    end
    nil
  end
end
