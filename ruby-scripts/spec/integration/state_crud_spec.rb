# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/interactors/state_crud"
require "mod_manager/adapters/catalog/memory"
require "mod_manager/adapters/modset_state/memory"
require "mod_manager/adapters/terminal/memory"

include ModManager

class StateCrudIntegrationTest < Minitest::Test
  def setup
    @catalog      = Adapters::Catalog::Memory.new
    @modset_state = Adapters::ModsetState::Memory.new
    @terminal     = Adapters::Terminal::Memory.new

    @catalog.seed_modset("main", collections: [])
  end

  def crud = Interactors::StateCrud.new(catalog: @catalog, modset_state: @modset_state, terminal: @terminal)

  def test_show_raises_on_unknown_modset
    assert_raises(Error) { crud.show("no-such") }
  end

  def test_show_prints_no_state_when_empty
    crud.show("main")
    assert_match(/no state/, @terminal.output)
  end

  def test_show_prints_redirect_and_overlay_sections
    @modset_state.seed("main",
      redirect: [{path: "mods/native/db.sqlite3", bytes: 4_200_000}],
      overlay:  [{path: "bin/config.json",        bytes: 1_000_000}])
    crud.show("main")
    assert_match(/redirect.*link farm/i, @terminal.output)
    assert_match(/overlay.*upper/i,      @terminal.output)
    assert_match(/db\.sqlite3/,          @terminal.output)
    assert_match(/config\.json/,         @terminal.output)
    assert_match(/total/,                @terminal.output)
  end

  def test_clean_raises_on_unknown_modset
    assert_raises(Error) { crud.clean("no-such") }
  end

  def test_clean_no_ops_when_no_state
    crud.clean("main", yes: true)
    assert_match(/no state/, @terminal.output)
    assert_empty @modset_state.wiped
  end

  def test_clean_confirms_and_wipes
    @modset_state.seed("main", redirect: [{path: "db.sqlite3", bytes: 1024}])
    @terminal.stub_confirm(true)
    crud.clean("main")
    assert_includes @modset_state.wiped, "main"
    assert_match(/wiped/, @terminal.output)
  end

  def test_clean_aborts_when_not_confirmed
    @modset_state.seed("main", redirect: [{path: "db.sqlite3", bytes: 1024}])
    @terminal.stub_confirm(false)
    crud.clean("main")
    assert_empty @modset_state.wiped
    assert_match(/aborted/, @terminal.output)
  end

  def test_clean_skips_confirm_with_yes_flag
    @modset_state.seed("main", redirect: [{path: "db.sqlite3", bytes: 1024}])
    crud.clean("main", yes: true)
    assert_includes @modset_state.wiped, "main"
  end
end
