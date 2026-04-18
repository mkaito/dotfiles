# frozen_string_literal: true

require "minitest/autorun"
require_relative "../../lib/mod_manager/interactors/cleanup"
require_relative "../../lib/mod_manager/adapters/catalog/memory"
require_relative "../../lib/mod_manager/adapters/mod_archive/memory"
require_relative "../../lib/mod_manager/adapters/terminal/memory"

include ModManager

class CleanupIntegrationTest < Minitest::Test
  def setup
    @catalog  = Adapters::Catalog::Memory.new
    @archive  = Adapters::ModArchive::Memory.new
    @terminal = Adapters::Terminal::Memory.new

    @archive.seed("referenced-mod")
    @archive.seed("orphan-mod")
    @catalog.seed_collection("core", mods: %w[referenced-mod])
  end

  def interactor = Interactors::Cleanup.new(archive: @archive, catalog: @catalog, terminal: @terminal)

  def test_reports_orphans
    interactor.call(dry_run: true)
    assert_match(/orphan-mod/, @terminal.output)
  end

  def test_dry_run_does_not_delete
    interactor.call(dry_run: true)
    assert @archive.include?("orphan-mod")
  end

  def test_deletes_orphans_when_confirmed
    @terminal.stub_confirm(true)
    interactor.call
    refute @archive.include?("orphan-mod")
    assert @archive.include?("referenced-mod")
  end

  def test_aborts_when_not_confirmed
    @terminal.stub_confirm(false)
    interactor.call
    assert @archive.include?("orphan-mod")
    assert_match(/aborted/, @terminal.output)
  end

  def test_skips_confirmation_with_yes_flag
    interactor.call(yes: true)
    refute @archive.include?("orphan-mod")
  end

  def test_reports_no_orphans_when_all_referenced
    @archive = Adapters::ModArchive::Memory.new
    @archive.seed("referenced-mod")
    interactor = Interactors::Cleanup.new(archive: @archive, catalog: @catalog, terminal: @terminal)
    interactor.call
    assert_match(/no orphaned/, @terminal.output)
  end
end
