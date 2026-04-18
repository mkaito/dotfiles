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

    make_mod("mod-a")
    make_mod("mod-b")

    @archive = Archive.new(@archive_dir)
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_add_appends_slug
    col_path = write_col("mylist", %w[mod-a])
    CollectionEditor.add(col_path, "mod-b", @archive)
    assert_equal %w[mod-a mod-b], Collection.load(col_path).mods
  end

  def test_add_multiple_slugs
    col_path = write_col("mylist", %w[])
    CollectionEditor.add(col_path, %w[mod-a mod-b], @archive)
    assert_equal %w[mod-a mod-b], Collection.load(col_path).mods
  end

  def test_add_skips_already_present
    col_path = write_col("mylist", %w[mod-a mod-b])
    CollectionEditor.add(col_path, "mod-b", @archive)
    assert_equal %w[mod-a mod-b], Collection.load(col_path).mods
  end

  def test_add_raises_when_slug_not_in_archive
    col_path = write_col("mylist", %w[])
    assert_raises(Error) { CollectionEditor.add(col_path, "no-such-mod", @archive) }
  end

  def test_remove_drops_slug
    col_path = write_col("mylist", %w[mod-a mod-b])
    CollectionEditor.remove(col_path, "mod-a")
    assert_equal %w[mod-b], Collection.load(col_path).mods
  end

  def test_remove_no_op_when_slug_absent
    col_path = write_col("mylist", %w[mod-a])
    CollectionEditor.remove(col_path, "no-such-mod")
    assert_equal %w[mod-a], Collection.load(col_path).mods
  end

  private

  def make_mod(slug)
    dir = File.join(@archive_dir, "cp2077", slug)
    FileUtils.mkdir_p(dir)
    File.write(File.join(dir, "meta.toml"), <<~TOML)
      name = "#{slug}"
      slug = "#{slug}"
      version = "1.0"
      game = "cp2077"
      depends = []
    TOML
  end

  def write_col(name, mods)
    path = File.join(@collections_dir, "#{name}.toml")
    File.write(path, TomlRB.dump("name" => name, "mods" => mods))
    path
  end
end
