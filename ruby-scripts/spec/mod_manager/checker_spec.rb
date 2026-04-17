# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require_relative "../../lib/mod_manager/checker"
require_relative "../../lib/mod_manager/archive"
require_relative "../../lib/mod_manager/modset"
require_relative "../support/fake_config"

include ModManager

class CheckerTest < Minitest::Test
  def setup
    @dir             = Dir.mktmpdir
    @archive_dir     = File.join(@dir, "archive")
    @collections_dir = File.join(@dir, "collections")
    @modsets_dir     = File.join(@dir, "modsets")
    [@archive_dir, @collections_dir, @modsets_dir].each { FileUtils.mkdir_p(_1) }

    make_mod("redscript", "2.0")
    make_mod("codeware",  "1.0", depends: %w[redscript])

    @config  = FakeConfig.new(archive_dir: @archive_dir, collections_dir: @collections_dir,
                              modsets_dir: @modsets_dir, game_dir: @dir, domain: "cyberpunk2077")
    @archive = Archive.new(@archive_dir)
    @checker = Checker.new(@archive, @config)
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_clean_modset_returns_no_errors
    write_col("base", %w[redscript codeware])
    ms = write_modset(collections: %w[base])
    assert_empty @checker.check_modset(ms)
  end

  def test_missing_slug_in_collection
    write_col("bad", %w[redscript no-such-mod])
    ms = write_modset(collections: %w[bad])
    errors = @checker.check_modset(ms)
    assert errors.any? { _1.include?("no-such-mod") }
  end

  def test_missing_collection_file
    ms = write_modset(collections: %w[nonexistent])
    errors = @checker.check_modset(ms)
    assert errors.any? { _1.include?("nonexistent") }
  end

  def test_missing_dep_reported
    make_mod("trace", "1.0", depends: %w[missing-dep])
    write_col("col", %w[trace])
    ms = write_modset(collections: %w[col])
    errors = @checker.check_modset(ms)
    assert errors.any? { _1.include?("missing-dep") }
  end

  def test_all_errors_reported_not_just_first
    write_col("multi-bad", %w[gone-a gone-b])
    ms = write_modset(collections: %w[multi-bad])
    errors = @checker.check_modset(ms)
    assert errors.any? { _1.include?("gone-a") }
    assert errors.any? { _1.include?("gone-b") }
  end

  def test_dependency_cycle_reported_as_error
    make_mod("a", "1.0", depends: %w[b])
    make_mod("b", "1.0", depends: %w[a])
    write_col("cycle-col", %w[a b])
    ms = write_modset(collections: %w[cycle-col])
    errors = @checker.check_modset(ms)
    assert errors.any? { _1.include?("cycle") }
  end

  def test_check_all_collects_errors_from_all_collections_and_modsets
    make_mod("ok-mod", "1.0")
    write_col("good",  %w[ok-mod])
    write_col("bad",   %w[missing-mod])
    write_modset(collections: %w[good])
    errors = @checker.check_all
    assert errors.any? { _1.include?("missing-mod") }
  end

  def test_check_all_reports_invalid_collection_toml
    File.write(File.join(@collections_dir, "broken.toml"), "game = \"cp2077\"\n# no mods key")
    errors = @checker.check_all
    assert errors.any? { _1.include?("broken") }
  end

  private

  def make_mod(slug, version, depends: [])
    dir = File.join(@archive_dir, slug, version)
    FileUtils.mkdir_p(File.join(dir, "files"))
    File.write(File.join(dir, "meta.toml"), <<~TOML)
      name = "#{slug}"
      slug = "#{slug}"
      version = "#{version}"
      game = "cp2077"
      depends = #{depends.inspect}
    TOML
  end

  def write_col(name, mods)
    File.write(File.join(@collections_dir, "#{name}.toml"), <<~TOML)
      name = "#{name}"
      game = "cp2077"
      mods = #{mods.inspect}
    TOML
  end

  def write_modset(collections: [], mods: [])
    path = File.join(@modsets_dir, "#{collections.first || "test"}.toml")
    File.write(path, <<~TOML)
      game = "cp2077"
      collections = #{collections.inspect}
      mods = #{mods.inspect}
    TOML
    Modset.load(path)
  end
end
