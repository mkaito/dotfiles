# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/interactors/verify_catalog"
require "mod_manager/adapters/catalog/memory"
require "mod_manager/adapters/mod_archive/memory"
require "mod_manager/adapters/terminal/memory"

include ModManager

class VerifyCatalogIntegrationTest < Minitest::Test
  def setup
    @catalog = Adapters::Catalog::Memory.new
    @archive = Adapters::ModArchive::Memory.new
    @terminal = Adapters::Terminal::Memory.new

    @archive.seed("redscript")
    @archive.seed("codeware")
    @catalog.seed_collection("frameworks", mods: %w[redscript codeware])
    @catalog.seed_modset("full", collections: %w[frameworks])
  end

  def vc = Interactors::VerifyCatalog.new(catalog: @catalog, archive: @archive, terminal: @terminal)

  # ── mode: :collections ────────────────────────────────────────────────────

  def test_collections_clean_reports_ok
    assert vc.call(names: [], mode: :collections)
    assert_match(/frameworks.*ok/, @terminal.output)
  end

  def test_collections_missing_slug_reports_error
    @catalog.seed_collection("broken", mods: %w[missing-mod])
    refute vc.call(names: [], mode: :collections)
    assert @terminal.errors.any? { it.include?("missing-mod") }
  end

  def test_collections_specific_names
    assert vc.call(names: %w[frameworks], mode: :collections)
  end

  def test_collections_unknown_name_reports_error
    refute vc.call(names: %w[no-such], mode: :collections)
    assert @terminal.errors.any? { it.include?("no-such") }
  end

  # ── mode: :modsets ────────────────────────────────────────────────────────

  def test_modsets_clean_reports_ok
    assert vc.call(names: [], mode: :modsets)
    assert_match(/full.*ok/, @terminal.output)
  end

  def test_modsets_missing_collection_reports_error
    @catalog.seed_modset("broken", collections: %w[nonexistent])
    refute vc.call(names: [], mode: :modsets)
    assert @terminal.errors.any? { it.include?("nonexistent") }
  end

  # ── mode: :all ────────────────────────────────────────────────────────────

  def test_all_clean_checks_both_kinds
    assert vc.call(names: [], mode: :all)
    assert_match(/frameworks.*ok/, @terminal.output)
    assert_match(/full.*ok/, @terminal.output)
  end

  def test_all_returns_false_when_any_collection_broken
    @archive.seed("extra")
    @catalog.seed_collection("extra-col", mods: %w[extra gone])
    refute vc.call(names: [], mode: :all)
    assert @terminal.errors.any? { it.include?("gone") }
  end

  def test_all_returns_false_when_any_modset_broken
    @catalog.seed_modset("bad", collections: %w[missing-col])
    refute vc.call(names: [], mode: :all)
    assert @terminal.errors.any? { it.include?("missing-col") }
  end

  # ── load errors reported, not swallowed ───────────────────────────────────

  def test_load_error_reported_not_swallowed
    # Simulate a catalog that raises ValidationError when a specific entry is read.
    # This covers the case where a TOML file is malformed on disk.
    bad_catalog = Class.new(Adapters::Catalog::Memory) do
      def read_collection(name)
        raise ValidationError, ["malformed: #{name}"] if name == "bad-toml"
        super
      end
    end.new
    bad_catalog.seed_collection("good", mods: [])
    bad_catalog.seed_collection("bad-toml", mods: [])

    terminal = Adapters::Terminal::Memory.new
    v = Interactors::VerifyCatalog.new(catalog: bad_catalog, archive: @archive, terminal:)
    refute v.call(names: [], mode: :collections)
    assert terminal.errors.any? { it.include?("bad-toml") }
    assert terminal.infos.any? { it.include?("good") }
  end
end
