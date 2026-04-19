# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/services/checker"
require "mod_manager/collection"
require "mod_manager/modset"
require "mod_manager/adapters/mod_archive/memory"

include ModManager

class CheckerTest < Minitest::Test
  def setup
    @archive = Adapters::ModArchive::Memory.new
    @archive.seed("redscript")
    @archive.seed("codeware")

    @col_a = Collection.new(name: "col-a", mods: %w[redscript codeware], path: "/fake/col-a.toml")
    @col_b = Collection.new(name: "col-b", mods: %w[missing-mod], path: "/fake/col-b.toml")
  end

  def test_check_collection_clean
    assert_empty Services::Checker.check_collection(@col_a, @archive)
  end

  def test_check_collection_missing_slug
    errors = Services::Checker.check_collection(@col_b, @archive)
    assert errors.any? { it.include?("missing-mod") }
  end

  def test_check_collection_all_errors_reported
    col = Collection.new(name: "multi", mods: %w[gone-a gone-b], path: "/fake/multi.toml")
    errors = Services::Checker.check_collection(col, @archive)
    assert errors.any? { it.include?("gone-a") }
    assert errors.any? { it.include?("gone-b") }
  end

  def test_check_modset_clean
    ms = Modset.new(game: "cp2077", collections: %w[col-a], mods: [], checks: [], path: "/fake/ms.toml")
    errors = Services::Checker.check_modset(ms, {"col-a" => @col_a}, @archive)
    assert_empty errors
  end

  def test_check_modset_missing_collection
    ms = Modset.new(game: "cp2077", collections: %w[nonexistent], mods: [], checks: [], path: "/fake/ms.toml")
    errors = Services::Checker.check_modset(ms, {}, @archive)
    assert errors.any? { it.include?("nonexistent") }
  end

  def test_check_modset_direct_mod_missing
    ms = Modset.new(game: "cp2077", collections: [], mods: %w[missing-direct], checks: [], path: "/fake/ms.toml")
    errors = Services::Checker.check_modset(ms, {}, @archive)
    assert errors.any? { it.include?("missing-direct") }
  end

  def test_check_all_collects_from_collections_and_modsets
    ms = Modset.new(game: "cp2077", collections: %w[col-b], mods: [], checks: [], path: "/fake/ms.toml")
    errors = Services::Checker.check_all([@col_a, @col_b], [ms], @archive)
    assert errors.any? { it.include?("missing-mod") }
  end
end
