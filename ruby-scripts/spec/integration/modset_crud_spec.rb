# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/interactors/modset_crud"
require "mod_manager/adapters/catalog/memory"
require "mod_manager/adapters/terminal/memory"

include ModManager

class ModsetCrudIntegrationTest < Minitest::Test
  def setup
    @catalog  = Adapters::Catalog::Memory.new
    @terminal = Adapters::Terminal::Memory.new

    @catalog.seed_collection("col-a", mods: %w[mod-a])
    @catalog.seed_collection("col-b", mods: %w[mod-b])
    @catalog.seed_modset("existing", collections: %w[col-a])
  end

  def crud = Interactors::ModsetCrud.new(catalog: @catalog, terminal: @terminal, game: "cp2077", modsets_dir: "/fake")

  def test_new_modset_creates_empty_modset
    crud.new_modset("newset")
    assert @catalog.modset_exist?("newset")
    assert_equal [], @catalog.read_modset("newset").collections
  end

  def test_new_modset_raises_if_already_exists
    assert_raises(Error) { crud.new_modset("existing") }
  end

  def test_list_prints_modsets
    crud.list
    assert_match(/existing/, @terminal.output)
  end

  def test_show_lists_collections
    crud.show("existing")
    assert_match(/col-a/, @terminal.output)
  end

  def test_add_appends_collection
    crud.add("existing", %w[col-b])
    assert_includes @catalog.read_modset("existing").collections, "col-b"
  end

  def test_remove_drops_collection
    crud.remove("existing", "col-a")
    refute_includes @catalog.read_modset("existing").collections, "col-a"
  end

  def test_delete_removes_modset_when_confirmed
    @terminal.stub_confirm(true)
    crud.delete("existing")
    refute @catalog.modset_exist?("existing")
  end

  def test_delete_aborts_when_not_confirmed
    @terminal.stub_confirm(false)
    crud.delete("existing")
    assert @catalog.modset_exist?("existing")
  end

  def test_delete_raises_when_not_found
    assert_raises(Error) { crud.delete("no-such") }
  end
end
