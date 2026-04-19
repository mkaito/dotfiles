# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require "toml-rb"
require "mod_manager/adapters/mod_archive/filesystem"

class FilesystemVersionSortingTest < Minitest::Test
  def setup
    @dir = Dir.mktmpdir
    @fs  = ModManager::Adapters::ModArchive::Filesystem.new(@dir)
  end

  def teardown = FileUtils.rm_rf(@dir)

  def make_version(dir_slug, slug, version)
    dir = File.join(@dir, "cp2077", dir_slug)
    FileUtils.mkdir_p(dir)
    File.write(File.join(dir, "meta.toml"), TomlRB.dump(
      "slug" => slug, "version" => version, "name" => slug,
      "game" => "cp2077", "depends" => [], "source" => {}
    ))
  end

  def test_lexicographic_version_ordering
    # 1.10 must beat 1.9 — not "1" < "9" string comparison
    make_version("mymod-1.9",  "mymod", "1.9")
    make_version("mymod-1.10", "mymod", "1.10")
    assert_equal "1.10", @fs.latest("mymod").version
  end

  def test_release_beats_prerelease
    # 1.0 must beat 1.0-beta — Gem::Version treats suffixed versions as prerelease
    make_version("mymod-beta",    "mymod", "1.0-beta")
    make_version("mymod-release", "mymod", "1.0")
    assert_equal "1.0", @fs.latest("mymod").version
  end
end
