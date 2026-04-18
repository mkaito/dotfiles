# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require_relative "../../lib/mod_manager/deployer"
require_relative "../../lib/mod_manager/archive"

include ModManager

class DeployerTest < Minitest::Test
  def setup
    @dir         = Dir.mktmpdir
    @archive_dir = File.join(@dir, "archive")
    @game_dir    = File.join(@dir, "game")
    FileUtils.mkdir_p(@game_dir)

    make_mod("redscript-2.0", "2.0", "bin/x64/global.ini" => "cfg")
    make_mod("codeware-1.0",  "1.0", "r6/scripts/cw.reds" => "src")

    @archive  = Archive.new(@archive_dir)
    @deployer = Deployer.new(@game_dir, @archive_dir)
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_deploy_creates_symlinks_at_correct_paths
    @deployer.deploy([@archive.latest("redscript-2.0")])
    link = File.join(@game_dir, "bin/x64/global.ini")
    assert File.symlink?(link)
    assert_equal File.join(@archive_dir, "cp2077/redscript-2.0/bin/x64/global.ini"), File.readlink(link)
  end

  def test_undeploy_removes_archive_symlinks
    @deployer.deploy([@archive.latest("redscript-2.0")])
    @deployer.undeploy
    refute File.exist?(File.join(@game_dir, "bin/x64/global.ini"))
  end

  def test_undeploy_removes_empty_dirs
    @deployer.deploy([@archive.latest("redscript-2.0")])
    @deployer.undeploy
    refute File.directory?(File.join(@game_dir, "bin/x64"))
    refute File.directory?(File.join(@game_dir, "bin"))
  end

  def test_undeploy_preserves_dirs_with_real_files
    @deployer.deploy([@archive.latest("redscript-2.0")])
    native = File.join(@game_dir, "bin/x64/native.dll")
    File.write(native, "game file")
    @deployer.undeploy
    assert File.directory?(File.join(@game_dir, "bin/x64"))
  end

  def test_undeploy_preserves_non_archive_files
    non_mod = File.join(@game_dir, "bin/x64/native.dll")
    FileUtils.mkdir_p(File.dirname(non_mod))
    File.write(non_mod, "original")

    @deployer.deploy([@archive.latest("redscript-2.0")])
    @deployer.undeploy

    assert File.exist?(non_mod)
    refute File.symlink?(non_mod)
  end

  def test_deploy_is_idempotent
    mods = [@archive.latest("redscript-2.0")]
    r1 = @deployer.deploy(mods)
    r2 = @deployer.deploy(mods)
    assert_equal r1[:created], r2[:created]
    assert_equal 1, Dir.glob("#{@game_dir}/**/*").count { File.symlink?(_1) }
  end

  def test_status_detects_broken_links
    @deployer.deploy([@archive.latest("redscript-2.0")])
    FileUtils.rm_rf(File.join(@archive_dir, "cp2077", "redscript-2.0", "bin"))
    st = @deployer.status
    assert st.values.any? { _1[:broken].any? }
  end

  def test_last_writer_wins_on_conflict
    make_mod("mod-a-1.0", "1.0", "bin/x64/shared.cfg" => "from-a")
    make_mod("mod-b-1.0", "1.0", "bin/x64/shared.cfg" => "from-b")
    @deployer.deploy([@archive.latest("mod-a-1.0"), @archive.latest("mod-b-1.0")])
    link = File.join(@game_dir, "bin/x64/shared.cfg")
    assert File.symlink?(link)
    assert_includes File.readlink(link), "mod-b"
  end

  def test_deploy_raises_on_real_file_at_dst
    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    File.write(File.join(@game_dir, "bin/x64/global.ini"), "real game file")
    assert_raises(Error) { @deployer.deploy([@archive.latest("redscript-2.0")]) }
  end

  def test_status_groups_by_game_slug
    @deployer.deploy([@archive.latest("redscript-2.0"), @archive.latest("codeware-1.0")])
    st = @deployer.status
    assert st.key?("cp2077/redscript-2.0")
    assert st.key?("cp2077/codeware-1.0")
  end

  private

  def make_mod(slug, version, files = {})
    dir = File.join(@archive_dir, "cp2077", slug)
    files.each do |rel, content|
      path = File.join(dir, rel)
      FileUtils.mkdir_p(File.dirname(path))
      File.write(path, content)
    end
    File.write(File.join(dir, "meta.toml"), <<~TOML)
      name = "#{slug}"
      slug = "#{slug}"
      version = "#{version}"
      game = "cp2077"
      depends = []
    TOML
  end
end
