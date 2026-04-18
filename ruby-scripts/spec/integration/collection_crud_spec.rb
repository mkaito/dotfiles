# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/interactors/collection_crud"
require "mod_manager/adapters/catalog/memory"
require "mod_manager/adapters/mod_archive/memory"
require "mod_manager/adapters/terminal/memory"

include ModManager

class CollectionCrudIntegrationTest < Minitest::Test
  def setup
    @catalog  = Adapters::Catalog::Memory.new
    @archive  = Adapters::ModArchive::Memory.new
    @terminal = Adapters::Terminal::Memory.new
    @archive.seed("mod-a")
    @archive.seed("mod-b")
    @catalog.seed_collection("mylist", mods: %w[mod-a])
  end

  def crud = Interactors::CollectionCrud.new(catalog: @catalog, archive: @archive, terminal: @terminal)

  def test_new_collection_creates_empty_collection
    crud.new_collection("newlist")
    assert @catalog.collection_exist?("newlist")
    assert_equal [], @catalog.read_collection("newlist").mods
  end

  def test_new_collection_raises_if_already_exists
    assert_raises(Error) { crud.new_collection("mylist") }
  end

  def test_add_appends_slug
    crud.add("mylist", "mod-b")
    assert_equal %w[mod-a mod-b], @catalog.read_collection("mylist").mods
  end

  def test_add_raises_when_not_in_archive
    assert_raises(Error) { crud.add("mylist", "no-such-mod") }
  end

  def test_remove_drops_slug
    crud.remove("mylist", "mod-a")
    assert_equal [], @catalog.read_collection("mylist").mods
  end

  def test_list_prints_collections
    crud.list
    assert_match(/mylist/, @terminal.output)
  end

  def test_show_lists_mods
    crud.show("mylist")
    assert_match(/mod-a/, @terminal.output)
  end

  def test_delete_removes_collection
    @terminal.stub_confirm(true)
    crud.delete("mylist")
    refute @catalog.collection_exist?("mylist")
  end

  def test_delete_raises_when_not_found
    assert_raises(Error) { crud.delete("no-such") }
  end

  def test_delete_aborts_when_not_confirmed
    @terminal.stub_confirm(false)
    crud.delete("mylist")
    assert @catalog.collection_exist?("mylist")
  end
end
