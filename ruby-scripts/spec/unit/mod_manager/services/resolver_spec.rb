# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/services/resolver"
require "mod_manager/collection"
require "mod_manager/modset"
require "mod_manager/adapters/mod_archive/memory"

include ModManager

class ResolverTest < Minitest::Test
  def setup
    @archive = Adapters::ModArchive::Memory.new
    @archive.seed("redscript", game: "cp2077")
    @archive.seed("codeware",  game: "cp2077")
    @archive.seed("extra",     game: "cp2077")
  end

  def col(name, mods) = Collection.new(name:, mods:, path: "/fake/#{name}.toml")
  def ms(collections: [], mods: []) = Modset.new(game: "cp2077", collections:, mods:, checks: [], path: "/fake/ms.toml")

  def test_collection_expansion
    result, = Services::Resolver.resolve(ms(collections: %w[a]), { "a" => col("a", %w[redscript codeware]) }, @archive)
    assert_equal %w[redscript codeware], result.map(&:slug)
  end

  def test_ad_hoc_mods_appended_after_collections
    result, = Services::Resolver.resolve(ms(collections: %w[a], mods: %w[extra]), { "a" => col("a", %w[redscript]) }, @archive)
    assert_equal %w[redscript extra], result.map(&:slug)
  end

  def test_same_slug_deduped_no_conflict
    result, conflicts = Services::Resolver.resolve(
      ms(collections: %w[a b]),
      { "a" => col("a", %w[redscript codeware]), "b" => col("b", %w[redscript extra]) },
      @archive,
    )
    assert_equal %w[redscript codeware extra], result.map(&:slug)
    assert_empty conflicts
  end

  def test_version_conflict_keyed_on_mod_id
    @archive.seed("rs-old", game: "cp2077", source: { "provider" => "nexus", "mod_id" => 1 })
    @archive.seed("rs-new", game: "cp2077", source: { "provider" => "nexus", "mod_id" => 1 })
    result, conflicts = Services::Resolver.resolve(
      ms(collections: %w[a b]),
      { "a" => col("a", %w[rs-old]), "b" => col("b", %w[rs-new]) },
      @archive,
    )
    assert_equal %w[rs-new], result.map(&:slug)
    assert_includes conflicts, "nexus:1"
  end

  def test_raises_on_missing_slug
    assert_raises(Error) do
      Services::Resolver.resolve(ms(collections: %w[a]), { "a" => col("a", %w[no-such-mod]) }, @archive)
    end
  end

  def test_raises_on_missing_collection
    assert_raises(Error) do
      Services::Resolver.resolve(ms(collections: %w[missing]), {}, @archive)
    end
  end
end
