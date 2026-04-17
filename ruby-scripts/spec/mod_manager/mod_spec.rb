# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require_relative "../../lib/mod_manager/mod"

include ModManager

class ModLoadTest < Minitest::Test
  def setup
    @dir = Dir.mktmpdir
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_valid_mod_loads
    write_meta(slug: "redscript", version: "2.0", name: "Redscript", game: "cp2077")
    mod = Mod.load(@dir)
    assert_equal "redscript", mod.slug
    assert_equal "2.0", mod.version
  end

  def test_missing_slug_raises_validation_error
    write_meta(slug: "", version: "1.0", name: "Mod", game: "cp2077")
    assert_raises(ValidationError) { Mod.load(@dir) }
  end

  def test_missing_name_raises_validation_error
    write_meta(slug: "mod", version: "1.0", name: "", game: "cp2077")
    assert_raises(ValidationError) { Mod.load(@dir) }
  end

  def test_missing_version_raises_validation_error
    write_meta(slug: "mod", version: "  ", name: "Mod", game: "cp2077")
    assert_raises(ValidationError) { Mod.load(@dir) }
  end

  def test_missing_game_raises_validation_error
    write_meta(slug: "mod", version: "1.0", name: "Mod", game: "")
    assert_raises(ValidationError) { Mod.load(@dir) }
  end

  def test_all_missing_fields_collected
    write_meta(slug: "", version: "", name: "", game: "")
    err = assert_raises(ValidationError) { Mod.load(@dir) }
    assert_equal 4, err.errors.size
  end

  def test_missing_meta_toml_raises_error
    assert_raises(Error) { Mod.load(@dir) }
  end

  def test_files_lists_files_under_files_dir
    write_meta(slug: "mod", version: "1.0", name: "Mod", game: "cp2077")
    FileUtils.mkdir_p(File.join(@dir, "files/bin/x64"))
    File.write(File.join(@dir, "files/bin/x64/plugin.dll"), "")
    mod = Mod.load(@dir)
    assert_equal [File.join(@dir, "files/bin/x64/plugin.dll")], mod.files
  end

  def test_to_s
    write_meta(slug: "redscript", version: "2.0", name: "Redscript", game: "cp2077")
    assert_equal "redscript@2.0", Mod.load(@dir).to_s
  end

  private

  def write_meta(slug:, version:, name:, game:)
    File.write(File.join(@dir, "meta.toml"), <<~TOML)
      slug = "#{slug}"
      version = "#{version}"
      name = "#{name}"
      game = "#{game}"
      depends = []
    TOML
  end
end
