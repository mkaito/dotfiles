# frozen_string_literal: true

require "minitest/autorun"
require_relative "../../lib/mod_manager/interactors/validate"
require_relative "../../lib/mod_manager/adapters/catalog/memory"
require_relative "../../lib/mod_manager/adapters/mod_archive/memory"
require_relative "../../lib/mod_manager/adapters/terminal/memory"

include ModManager

class ValidateIntegrationTest < Minitest::Test
  def setup
    @catalog  = Adapters::Catalog::Memory.new
    @archive  = Adapters::ModArchive::Memory.new
    @terminal = Adapters::Terminal::Memory.new
    @archive.seed("mod-a")
    @catalog.seed_collection("good", mods: %w[mod-a])
    @catalog.seed_collection("bad",  mods: %w[missing-mod])
    @catalog.seed_modset("ms-good", collections: %w[good])
    @catalog.seed_modset("ms-bad",  collections: %w[bad])
  end

  def interactor = Interactors::Validate.new(catalog: @catalog, archive: @archive, terminal: @terminal)

  def test_valid_collection_returns_true
    assert interactor.call("good")
    assert_match(/ok/, @terminal.output)
  end

  def test_invalid_collection_returns_false
    refute interactor.call("bad")
    assert @terminal.errors.any?
  end

  def test_valid_modset_returns_true
    assert interactor.call("ms-good")
  end

  def test_invalid_modset_returns_false
    refute interactor.call("ms-bad")
  end

  def test_validate_all_reports_all_errors
    refute interactor.call
    assert @terminal.errors.any?
  end

  def test_raises_when_name_not_found
    assert_raises(Error) { interactor.call("no-such") }
  end
end
