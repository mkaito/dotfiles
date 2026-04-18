# frozen_string_literal: true

require "minitest/autorun"
# core/models first so collection.rb (loaded below) can override Collection with its Struct
require_relative "../../lib/mod_manager/core/models"
require_relative "../../lib/mod_manager/collection_revision"
require_relative "../../lib/mod_manager/interactors/import_collection"
require_relative "../../lib/mod_manager/adapters/collection_provider/memory"
require_relative "../../lib/mod_manager/adapters/download/memory"
require_relative "../../lib/mod_manager/adapters/mod_archive/memory"
require_relative "../../lib/mod_manager/adapters/catalog/memory"
require_relative "../../lib/mod_manager/adapters/terminal/memory"

include ModManager

class ImportCollectionIntegrationTest < Minitest::Test
  def setup
    @provider = Adapters::CollectionProvider::Memory.new
    @download = Adapters::Download::Memory.new
    @archive  = Adapters::ModArchive::Memory.new
    @catalog  = Adapters::Catalog::Memory.new
    @terminal = Adapters::Terminal::Memory.new

    @rev = CollectionRevision.new(
      collection_id:   "n0nymh",
      collection_name: "cet-essentials",
      revision_number: 47,
      mods: [
        CollectionRevisionMod.new(mod_id: 107,  file_id: 1001),
        CollectionRevisionMod.new(mod_id: 1511, file_id: 2001),
      ],
    )
    @provider.stub_revision("n0nymh", @rev)

    stub_mod(@download, @archive, 107,  1001, "nexus-107-1001-cet-1-0",       "1.0")
    stub_mod(@download, @archive, 1511, 2001, "nexus-1511-2001-redscript-1-0", "1.0")
  end

  def interactor
    Interactors::ImportCollection.new(
      provider: @provider, download: @download,
      archive: @archive, catalog: @catalog, terminal: @terminal,
    )
  end

  def test_imports_collection_and_writes_toml
    col = interactor.call("n0nymh")
    assert_equal "nexus-n0nymh-cet-essentials-47", col.name
    assert_equal 2, col.mods.size
    assert @catalog.collection_exist?("nexus-n0nymh-cet-essentials-47")
  end

  def test_collection_contains_installed_slugs
    interactor.call("n0nymh")
    col = @catalog.read_collection("nexus-n0nymh-cet-essentials-47")
    assert_includes col.mods, "nexus-107-1001-cet-1-0"
    assert_includes col.mods, "nexus-1511-2001-redscript-1-0"
  end

  def test_skips_already_installed_mods
    @archive.seed("nexus-107-1001-cet-1-0",
                  source: { "provider" => "nexus", "mod_id" => 107, "file_id" => 1001 })
    interactor.call("n0nymh")
    assert_equal 1, @download.fetched.size, "should only fetch the one missing mod"
    assert_includes @terminal.infos.join, "already archived"
  end

  def test_specific_revision
    rev2 = CollectionRevision.new(
      collection_id:   "n0nymh",
      collection_name: "cet-essentials",
      revision_number: 46,
      mods: [CollectionRevisionMod.new(mod_id: 107, file_id: 999)],
    )
    @provider.stub_revision("n0nymh", rev2)
    stub_mod(@download, @archive, 107, 999, "nexus-107-999-cet-old", "0.9")

    col = interactor.call("n0nymh", revision: 46)
    assert_equal "nexus-n0nymh-cet-essentials-46", col.name
  end

  private

  def stub_mod(download, archive, mod_id, file_id, slug, version)
    unpacked = UnpackedMod.new(
      tmp_dir: "/tmp/fake-#{slug}",
      slug:    slug,
      version: version,
      game:    "cyberpunk2077",
      name:    slug,
      source:  { "provider" => "nexus", "mod_id" => mod_id, "file_id" => file_id },
    )
    download.stub_fetch(mod_id, file_id, unpacked_mod: unpacked)
  end
end
