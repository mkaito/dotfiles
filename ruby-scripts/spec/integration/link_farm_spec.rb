# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require "toml-rb"
require "mod_manager/adapters/deploy/link_farm"
require "mod_manager/adapters/deploy/redirect_store"
require "mod_manager/services/game_profile/cyberpunk2077"
require "mod_manager/mod"

include ModManager

class LinkFarmTest < Minitest::Test
  def setup
    @dir         = Dir.mktmpdir
    @game_dir    = File.join(@dir, "game")
    @archive_dir = File.join(@dir, "archive")
    @data_dir    = File.join(@dir, "mod-data")
    FileUtils.mkdir_p([@game_dir, @archive_dir])
    @farm    = Adapters::Deploy::LinkFarm.new(@game_dir, @archive_dir)
    @farm_cp = Adapters::Deploy::LinkFarm.new(
      @game_dir, @archive_dir,
      game_profile: Services::GameProfile::Cyberpunk2077,
      redirects:    Adapters::Deploy::RedirectStore::CyberpunkRedirects
    )
  end

  def teardown = FileUtils.rm_rf(@dir)

  # ----------------------------------------------------------------
  # Helpers
  # ----------------------------------------------------------------

  def make_mod(slug, files)
    mod_dir = File.join(@archive_dir, "cp2077", slug)
    files.each do |rel|
      path = File.join(mod_dir, rel)
      FileUtils.mkdir_p(File.dirname(path))
      File.write(path, slug)
    end
    File.write(File.join(mod_dir, "meta.toml"), TomlRB.dump(
      "name" => slug, "slug" => slug, "version" => "1.0",
      "game" => "cp2077", "depends" => [], "source" => {}
    ))
    Mod.load(mod_dir)
  end

  def deploy(mods, modset: "test")
    @farm.deploy(mods: mods, modset: modset)
  end

  def deploy_cp(mods, modset: "test")
    @farm_cp.deploy(mods: mods, modset: modset)
  end

  # ----------------------------------------------------------------
  # File symlinks — single mod, flat file at root (no subdir → file symlink, not dir)
  # ----------------------------------------------------------------

  def test_single_mod_flat_file_creates_file_symlink
    mod = make_mod("cet", ["global.ini"])
    result = deploy([mod])

    link = File.join(@game_dir, "global.ini")
    assert_equal({ created: 1 }, result)
    assert File.symlink?(link)
    assert File.exist?(link)
    assert_includes File.readlink(link), @archive_dir
  end

  # Single mod with files in subdirs → dir symlink at highest exclusive node
  def test_single_mod_subdir_creates_dir_symlink
    mod = make_mod("cet", ["bin/x64/global.ini"])
    result = deploy([mod])

    dir_link = File.join(@game_dir, "bin")
    assert_equal({ created: 1 }, result)
    assert File.symlink?(dir_link), "expected dir symlink at game_dir/bin"
    assert File.exist?(File.join(@game_dir, "bin/x64/global.ini")), "file accessible through dir symlink"
  end

  # ----------------------------------------------------------------
  # Dir symlinks — CET-style: two mods each in unique subdirs
  # under a shared parent → parent is a real dir, children are dir symlinks
  # ----------------------------------------------------------------

  def test_cet_style_mods_get_directory_symlinks
    native = make_mod("native", ["bin/x64/plugins/cet/mods/nativeSettings/init.lua"])
    vhs    = make_mod("vhs",    ["bin/x64/plugins/cet/mods/vehicleHandling/init.lua"])
    result = deploy([native, vhs])

    assert_equal({ created: 2 }, result)

    native_link = File.join(@game_dir, "bin/x64/plugins/cet/mods/nativeSettings")
    vhs_link    = File.join(@game_dir, "bin/x64/plugins/cet/mods/vehicleHandling")

    assert File.symlink?(native_link), "nativeSettings should be a symlink"
    assert File.symlink?(vhs_link),    "vehicleHandling should be a symlink"

    assert_includes File.readlink(native_link), "native"
    assert_includes File.readlink(vhs_link),    "vhs"
  end

  # ----------------------------------------------------------------
  # Pre-flight: real file at destination → raises Error
  # ----------------------------------------------------------------

  def test_preflight_raises_on_real_file
    mod = make_mod("cet", ["bin/x64/global.ini"])
    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    File.write(File.join(@game_dir, "bin/x64/global.ini"), "real")

    assert_raises(Error) { deploy([mod]) }
  end

  # ----------------------------------------------------------------
  # Pre-flight: foreign symlink at destination → raises Error
  # ----------------------------------------------------------------

  def test_preflight_raises_on_foreign_symlink
    mod = make_mod("cet", ["bin/x64/global.ini"])
    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    File.symlink("/etc/hosts", File.join(@game_dir, "bin/x64/global.ini"))

    assert_raises(Error) { deploy([mod]) }
  end

  # ----------------------------------------------------------------
  # Redeploy: old symlinks (including dir symlinks) removed, new created
  # ----------------------------------------------------------------

  def test_redeploy_replaces_symlinks
    mod_a = make_mod("mod-a", ["bin/x64/a.dll"])
    mod_b = make_mod("mod-b", ["bin/x64/b.dll"])

    deploy([mod_a])
    assert File.symlink?(File.join(@game_dir, "bin")), "first deploy: dir symlink at bin"

    deploy([mod_b])
    refute File.exist?(File.join(@game_dir, "bin/x64/a.dll")), "old file gone after redeploy"
    assert File.symlink?(File.join(@game_dir, "bin")), "second deploy: new dir symlink at bin"
    assert File.exist?(File.join(@game_dir, "bin/x64/b.dll")), "new file accessible"
  end

  # ----------------------------------------------------------------
  # Undeploy: removes file and dir symlinks; removes empty dirs; leaves real files
  # ----------------------------------------------------------------

  def test_undeploy_removes_dir_symlinks_and_empty_dirs
    mod = make_mod("cet", ["bin/x64/plugins/cet/mods/nativeSettings/init.lua"])
    deploy([mod])

    link = File.join(@game_dir, "bin")
    assert File.symlink?(link), "expected dir symlink at game_dir/bin"

    result = @farm.undeploy
    assert result[:removed] >= 1
    refute File.exist?(File.join(@game_dir, "bin")), "dir symlink should be removed"
  end

  # When a real game file shares a directory with mod files, the sentinel logic
  # forces file-level symlinks. Undeploy removes symlinks, leaving the real file.
  def test_undeploy_removes_parent_of_sibling_dir_symlinks
    native = make_mod("native", ["mods/nativeSettings/init.lua"])
    vhs    = make_mod("vhs",    ["mods/vehicleHandling/init.lua"])
    deploy([native, vhs])

    assert File.symlink?(File.join(@game_dir, "mods/nativeSettings")),  "first child is a dir symlink"
    assert File.symlink?(File.join(@game_dir, "mods/vehicleHandling")), "second child is a dir symlink"
    assert File.directory?(File.join(@game_dir, "mods")),              "parent is a real dir"

    @farm.undeploy
    refute File.exist?(File.join(@game_dir, "mods")), "parent dir removed after both siblings unlinked"
  end

  def test_undeploy_preserves_real_files
    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    real = File.join(@game_dir, "bin/x64/native.dll")
    File.write(real, "real")

    mod = make_mod("cet", ["bin/x64/global.ini"])
    deploy([mod])

    assert File.symlink?(File.join(@game_dir, "bin/x64/global.ini")), "file symlink for mod file"
    assert File.file?(real), "real file untouched during deploy"

    @farm.undeploy
    assert File.file?(real), "real file must survive undeploy"
    refute File.exist?(File.join(@game_dir, "bin/x64/global.ini")), "mod symlink removed"
  end

  # ----------------------------------------------------------------
  # Status: correctly attributes dir symlinks to their mod
  # ----------------------------------------------------------------

  def test_status_attributes_dir_symlinks
    mod = make_mod("native", ["bin/x64/plugins/cet/mods/nativeSettings/init.lua"])
    deploy([mod])

    status = @farm.status
    assert status.any? { |k, _| k.include?("native") },
           "status should contain an entry for the native mod"
  end

  # ----------------------------------------------------------------
  # Sqlite redirect: writes go to data-dir, not archive
  # ----------------------------------------------------------------
  # Use two mods under a shared parent (CET-style) so each gets a dir symlink
  # at their own subdir level — matching how nativeSettings/vehicleHandling deploy.

  def test_sqlite_redirected_to_data_dir
    native = make_mod("native", ["mods/nativeSettings/init.lua"])
    vhs    = make_mod("vhs",    ["mods/vehicleHandling/init.lua"])
    deploy_cp([native, vhs], modset: "firstrun")

    # Each mod gets a dir symlink at its own subdir
    assert File.symlink?(File.join(@game_dir, "mods/nativeSettings")), "dir symlink created"

    # Archive has a redirect symlink for db.sqlite3 inside the nativeSettings subdir
    archive_db = File.join(@archive_dir, "cp2077/native/mods/nativeSettings/db.sqlite3")
    assert File.symlink?(archive_db), "archive has redirect symlink for db.sqlite3"

    target = File.expand_path(File.readlink(archive_db), File.dirname(archive_db))
    assert_includes target, "firstrun"
    assert_includes target, @data_dir

    # Simulate game writing through the dir symlink chain
    FileUtils.mkdir_p(File.dirname(target))
    File.write(target, "settings data")
    assert_equal "settings data", File.read(File.join(@game_dir, "mods/nativeSettings/db.sqlite3")),
                 "write through dir symlink → archive redirect → data_dir"
  end

  def test_sqlite_migrated_from_game_dir_before_deploy
    # Simulate leftover db.sqlite3 at the exact dir-symlink level (from old deploy)
    FileUtils.mkdir_p(File.join(@game_dir, "mods/nativeSettings"))
    old_db = File.join(@game_dir, "mods/nativeSettings/db.sqlite3")
    File.write(old_db, "old settings")

    native = make_mod("native", ["mods/nativeSettings/init.lua"])
    vhs    = make_mod("vhs",    ["mods/vehicleHandling/init.lua"])
    deploy_cp([native, vhs], modset: "firstrun")

    # The direct path is no longer a real file — it's accessible only through symlinks
    refute File.file?(File.join(@game_dir, "mods/nativeSettings")), "game_dir path is now a symlink, not a real dir"
    assert File.symlink?(File.join(@game_dir, "mods/nativeSettings")), "dir symlink created after migration"

    expected_data = File.join(@data_dir, "firstrun/mods/nativeSettings/db.sqlite3")
    assert File.exist?(expected_data), "db.sqlite3 in data_dir"
    assert_equal "old settings", File.read(expected_data)
    # The full chain resolves: game_dir → dir symlink → archive redirect → data_dir
    assert_equal "old settings", File.read(old_db), "accessible through symlink chain"
  end

  def test_sqlite_namespaced_per_modset
    native = make_mod("native", ["mods/nativeSettings/init.lua"])
    vhs    = make_mod("vhs",    ["mods/vehicleHandling/init.lua"])

    deploy_cp([native, vhs], modset: "preset-a")
    db_a = File.join(@data_dir, "preset-a/mods/nativeSettings/db.sqlite3")
    FileUtils.mkdir_p(File.dirname(db_a))
    File.write(db_a, "preset-a settings")

    @farm_cp.undeploy
    deploy_cp([native, vhs], modset: "preset-b")
    db_b = File.join(@data_dir, "preset-b/mods/nativeSettings/db.sqlite3")
    FileUtils.mkdir_p(File.dirname(db_b))
    File.write(db_b, "preset-b settings")

    @farm_cp.undeploy
    deploy_cp([native, vhs], modset: "preset-a")
    archive_db = File.join(@archive_dir, "cp2077/native/mods/nativeSettings/db.sqlite3")
    target = File.expand_path(File.readlink(archive_db), File.dirname(archive_db))
    assert_includes target, "preset-a", "redirect points to preset-a data dir"
    assert_equal "preset-a settings", File.read(db_a), "preset-a settings preserved"
    assert_equal "preset-b settings", File.read(db_b), "preset-b settings preserved independently"
  end

  def test_ephemeral_variants_deleted_on_undeploy
    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    FileUtils.mkdir_p(File.join(@game_dir, "r6/cache"))

    files = {
      "bin/x64/Plugin.log?"              => "log-current",
      "bin/x64/vkd3d-proton.cache.write" => "vkd3d",
      "r6/cache/final.redscripts.modded" => "modded",
      "r6/cache/final.redscripts.ts"     => "ts",
    }
    files.each { |rel, content| File.write(File.join(@game_dir, rel), content) }

    @farm_cp.undeploy

    files.each_key do |rel|
      refute File.exist?(File.join(@game_dir, rel)), "#{rel} should be deleted on undeploy"
    end
  end

  def test_ephemerals_deleted_before_deploy
    mod = make_mod("cet", ["bin/x64/global.ini"])
    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    log = File.join(@game_dir, "bin/x64/plugin.log")
    File.write(log, "stale")

    deploy_cp([mod])
    refute File.exist?(log), ".log deleted by pre_deploy_cleanup"
  end

  def test_logs_deleted_on_undeploy
    # Simulate a .log file left in game_dir after a deploy
    FileUtils.mkdir_p(File.join(@game_dir, "mods/nativeSettings"))
    log = File.join(@game_dir, "mods/nativeSettings/nativeSettings.log")
    File.write(log, "log content")

    @farm_cp.undeploy
    refute File.exist?(log), ".log file deleted by undeploy"
  end

  # ----------------------------------------------------------------
  # CET config redirect: fresh install gets redirects for bindings.json etc.
  # Use two mods so cyber_engine_tweaks itself gets a dir symlink, not just its children.
  # ----------------------------------------------------------------

  def test_cet_config_redirected_to_data_dir
    # CET owns cyber_engine_tweaks exclusively; a sibling ASI mod shares bin/x64/plugins/
    # so the solver creates a dir symlink at cyber_engine_tweaks (not at bin/).
    cet      = make_mod("cet",       ["bin/x64/plugins/cyber_engine_tweaks/scripts/main.lua"])
    other    = make_mod("other-asi", ["bin/x64/plugins/other.asi"])
    deploy_cp([cet, other], modset: "fresh")

    cet_link = File.join(@game_dir, "bin/x64/plugins/cyber_engine_tweaks")
    assert File.symlink?(cet_link), "cyber_engine_tweaks should be a dir symlink on fresh install"

    # Archive should have redirect symlinks for CET config files
    archive_cet_dir = File.join(@archive_dir, "cp2077/cet/bin/x64/plugins/cyber_engine_tweaks")
    %w[bindings.json config.json layout.ini persistent.json].each do |cfg|
      archive_cfg = File.join(archive_cet_dir, cfg)
      assert File.symlink?(archive_cfg), "archive redirect missing for #{cfg}"
      target = File.expand_path(File.readlink(archive_cfg), archive_cet_dir)
      assert_includes target, "fresh",   "redirect points to modset-namespaced path"
      assert_includes target, @data_dir, "redirect points into data_dir"
    end

    # Simulate CET writing bindings.json through the dir symlink chain
    archive_bindings = File.join(archive_cet_dir, "bindings.json")
    bindings_data_path = File.expand_path(File.readlink(archive_bindings), archive_cet_dir)
    FileUtils.mkdir_p(File.dirname(bindings_data_path))
    File.write(bindings_data_path, "overlay_key=F2")
    assert_equal "overlay_key=F2",
                 File.read(File.join(@game_dir, "bin/x64/plugins/cyber_engine_tweaks/bindings.json")),
                 "write through dir symlink → archive redirect → data_dir"
  end

  # ----------------------------------------------------------------
  # Error propagation: lib raises instead of silently returning []
  # ----------------------------------------------------------------

  def test_non_archive_content_raises_on_unreadable_dir
    mod = make_mod("cet", ["bin/x64/global.ini"])
    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    File.write(File.join(@game_dir, "bin/x64/global.ini"), "real")

    # Make the directory untraversable
    FileUtils.chmod(0o000, File.join(@game_dir, "bin/x64"))
    begin
      assert_raises(Errno::EACCES) { deploy([mod]) }
    ensure
      FileUtils.chmod(0o755, File.join(@game_dir, "bin/x64"))
    end
  end

  def test_ensure_data_redirect_raises_on_unwritable_archive_dir
    native = make_mod("native", ["mods/nativeSettings/init.lua"])
    vhs    = make_mod("vhs",    ["mods/vehicleHandling/init.lua"])

    archive_native = File.join(@archive_dir, "cp2077/native/mods/nativeSettings")
    FileUtils.mkdir_p(archive_native)
    FileUtils.chmod(0o555, archive_native)
    begin
      assert_raises(Errno::EACCES) { deploy_cp([native, vhs], modset: "firstrun") }
    ensure
      FileUtils.chmod(0o755, archive_native)
    end
  end
end
