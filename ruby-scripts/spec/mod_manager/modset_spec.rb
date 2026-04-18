# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require "toml-rb"
require_relative "../../lib/mod_manager/modset"
require_relative "../../lib/mod_manager/archive"
require_relative "../support/fake_config"

include ModManager

class ModsetResolveTest < Minitest::Test
  def setup
    @dir             = Dir.mktmpdir
    @archive_dir     = File.join(@dir, "archive")
    @collections_dir = File.join(@dir, "collections")
    [@archive_dir, @collections_dir].each { FileUtils.mkdir_p(_1) }

    make_mod("redscript", "2.0")
    make_mod("codeware",  "1.0")
    make_mod("extra",     "1.0")

    @config  = FakeConfig.new(archive_dir: @archive_dir, collections_dir: @collections_dir,
                              modsets_dir: @dir, game_dir: @dir, domain: "cyberpunk2077")
    @archive = Archive.new(@archive_dir)
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_collection_expansion
    write_col("base", %w[redscript codeware])
    ms = write_modset(collections: %w[base])
    assert_equal %w[redscript codeware], ms.resolve(@archive, @config).first.map(&:slug)
  end

  def test_ad_hoc_mods_appended_after_collections
    write_col("base", %w[redscript])
    ms = write_modset(collections: %w[base], mods: %w[extra])
    assert_equal %w[redscript extra], ms.resolve(@archive, @config).first.map(&:slug)
  end

  def test_same_slug_in_two_collections_is_idempotent
    write_col("a", %w[redscript codeware])
    write_col("b", %w[redscript extra])
    ms = write_modset(collections: %w[a b])
    mods, conflicts = ms.resolve(@archive, @config)
    assert_equal %w[redscript codeware extra], mods.map(&:slug)
    assert_empty conflicts
  end

  def test_version_conflict_keyed_on_mod_id
    make_mod("redscript-0.20", "0.20", mod_id: 12345)
    make_mod("redscript-0.21", "0.21", mod_id: 12345)
    write_col("a", %w[redscript-0.20])
    write_col("b", %w[redscript-0.21])
    ms = write_modset(collections: %w[a b])
    mods, conflicts = ms.resolve(@archive, @config)
    assert_equal %w[redscript-0.21], mods.map(&:slug)
    assert_includes conflicts, "nexus:12345"
    assert_equal %w[redscript-0.20 redscript-0.21], conflicts["nexus:12345"]
  end

  def test_collections_applied_in_order
    write_col("first",  %w[redscript])
    write_col("second", %w[codeware])
    ms = write_modset(collections: %w[first second])
    assert_equal %w[redscript codeware], ms.resolve(@archive, @config).first.map(&:slug)
  end

  def test_resolve_raises_on_unresolved_atom
    write_col("base", %w[no-such-mod])
    ms = write_modset(collections: %w[base])
    assert_raises(Error) { ms.resolve(@archive, @config) }
  end

  def test_resolve_raises_on_missing_collection
    ms = write_modset(collections: %w[nonexistent])
    assert_raises(Error) { ms.resolve(@archive, @config) }
  end

  private

  def make_mod(slug, version = "1.0", mod_id: nil)
    dir = File.join(@archive_dir, "cp2077", slug)
    FileUtils.mkdir_p(dir)
    source = mod_id ? "\n[source]\nprovider = \"nexus\"\nmod_id = #{mod_id}\n" : ""
    File.write(File.join(dir, "meta.toml"), <<~TOML)
      name = "#{slug}"
      slug = "#{slug}"
      version = "#{version}"
      game = "cp2077"
      depends = []#{source}
    TOML
  end

  def write_col(name, mods)
    File.write(File.join(@collections_dir, "#{name}.toml"),
               TomlRB.dump("name" => name, "mods" => mods))
  end

  def write_modset(collections: [], mods: [])
    path = File.join(@dir, "test.toml")
    File.write(path, <<~TOML)
      game = "cp2077"
      collections = #{collections.inspect}
      mods = #{mods.inspect}
    TOML
    Modset.load(path)
  end
end
