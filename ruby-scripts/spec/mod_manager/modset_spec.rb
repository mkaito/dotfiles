# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
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

  def test_deduplication_first_occurrence_wins
    write_col("a", %w[redscript codeware])
    write_col("b", %w[redscript extra])
    ms = write_modset(collections: %w[a b])
    assert_equal %w[redscript codeware extra], ms.resolve(@archive, @config).first.map(&:slug)
  end

  def test_collections_applied_in_order
    write_col("first",  %w[redscript])
    write_col("second", %w[codeware])
    ms = write_modset(collections: %w[first second])
    assert_equal %w[redscript codeware], ms.resolve(@archive, @config).first.map(&:slug)
  end

  private

  def make_mod(slug, version)
    dir = File.join(@archive_dir, slug, version)
    FileUtils.mkdir_p(File.join(dir, "files"))
    File.write(File.join(dir, "meta.toml"), <<~TOML)
      name = "#{slug}"
      slug = "#{slug}"
      version = "#{version}"
      game = "cp2077"
      depends = []
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
    path = File.join(@dir, "test.toml")
    File.write(path, <<~TOML)
      game = "cp2077"
      collections = #{collections.inspect}
      mods = #{mods.inspect}
    TOML
    Modset.load(path)
  end
end
