# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require_relative "../../lib/mod_manager/collection_editor"
require_relative "../../lib/mod_manager/archive"

include ModManager

class CollectionEditorTest < Minitest::Test
  def setup
    @dir             = Dir.mktmpdir
    @archive_dir     = File.join(@dir, "archive")
    @collections_dir = File.join(@dir, "collections")
    FileUtils.mkdir_p([@archive_dir, @collections_dir])

    make_mod("base",   "1.0")
    make_mod("dep",    "1.0")
    make_mod("mod-a",  "1.0", depends: %w[dep])

    @archive = Archive.new(@archive_dir)
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_add_inserts_dep_before_mod
    col_path = write_col("mylist", %w[base])
    CollectionEditor.add(col_path, "mod-a", @archive)
    col = Collection.load(col_path)
    dep_i   = col.mods.index("dep")
    mod_a_i = col.mods.index("mod-a")
    assert dep_i, "dep not in collection"
    assert mod_a_i, "mod-a not in collection"
    assert dep_i < mod_a_i, "dep must come before mod-a"
  end

  def test_add_skips_already_present
    col_path = write_col("mylist", %w[base dep mod-a])
    CollectionEditor.add(col_path, "mod-a", @archive)
    col = Collection.load(col_path)
    assert_equal %w[base dep mod-a], col.mods
  end

  def test_remove_drops_slug
    col_path = write_col("mylist", %w[base dep mod-a])
    CollectionEditor.remove(col_path, "mod-a", @archive)
    col = Collection.load(col_path)
    refute_includes col.mods, "mod-a"
    assert_includes col.mods, "base"
  end

  def test_remove_warns_on_dependents
    col_path = write_col("mylist", %w[dep mod-a])
    out, = capture_io { CollectionEditor.remove(col_path, "dep", @archive) }
    assert_includes out, "mod-a"
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
    path = File.join(@collections_dir, "#{name}.toml")
    File.write(path, <<~TOML)
      name = "#{name}"
      game = "cp2077"
      mods = #{mods.inspect}
    TOML
    path
  end
end
