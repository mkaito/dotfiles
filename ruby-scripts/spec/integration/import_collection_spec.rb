# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
# core/models first so collection.rb (loaded below) can override Collection with its Struct
require "mod_manager/core/models"
require "mod_manager/collection_revision"
require "mod_manager/interactors/import_collection"
require "mod_manager/adapters/collection_provider/memory"
require "mod_manager/adapters/download/memory"
require "mod_manager/adapters/mod_archive/memory"
require "mod_manager/adapters/catalog/memory"
require "mod_manager/adapters/terminal/memory"

FOMOD_FIXTURE_XML = File.join(__dir__, "../fixtures/nexus/fomod_moduleconfig.xml")

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
    @provider.stub_manifest("n0nymh", [])

    stub_mod(@download, @archive, 107,  1001, "nexus-107-1001-cet-1-0",       "1.0")
    stub_mod(@download, @archive, 1511, 2001, "nexus-1511-2001-redscript-1-0", "1.0")
  end

  def interactor
    Interactors::ImportCollection.new(
      provider: @provider, download: @download,
      archive: @archive, catalog: @catalog, terminal: @terminal,
      game: "cyberpunk2077",
    )
  end

  def test_imports_collection_and_writes_toml
    interactor.call("n0nymh")
    assert @catalog.collection_exist?("nexus-n0nymh-cet-essentials-47")
    col = @catalog.read_collection("nexus-n0nymh-cet-essentials-47")
    assert_equal 2, col.mods.size
  end

  def test_collection_contains_archived_slugs
    interactor.call("n0nymh")
    col = @catalog.read_collection("nexus-n0nymh-cet-essentials-47")
    assert_includes col.mods, "nexus-107-1001-cet-1-0"
    assert_includes col.mods, "nexus-1511-2001-redscript-1-0"
  end

  def test_writes_modset_for_collection
    interactor.call("n0nymh")
    assert @catalog.modset_exist?("nexus-n0nymh-cet-essentials-47")
    ms = @catalog.read_modset("nexus-n0nymh-cet-essentials-47")
    assert_equal ["nexus-n0nymh-cet-essentials-47"], ms.collections
    assert_equal "cyberpunk2077", ms.game
  end

  def test_skips_already_archived_mods
    @archive.seed("nexus-107-1001-cet-1-0",
                  source: { "provider" => "nexus", "mod_id" => 107, "file_id" => 1001 })
    interactor.call("n0nymh")
    assert_equal 1, @download.fetched.size, "should only fetch the one missing mod"
    assert_includes @terminal.infos.join, "already archived"
  end

  def test_aborts_if_file_missing_from_provider
    # Remove the stub for mod 1511 so file_exist? returns false
    @download = Adapters::Download::Memory.new
    stub_mod(@download, @archive, 107, 1001, "nexus-107-1001-cet-1-0", "1.0")
    # mod 1511 / file 2001 intentionally NOT stubbed

    err = assert_raises(Core::Error) { interactor.call("n0nymh") }
    assert_match(/no longer available/, err.message)
    assert_equal 0, @download.fetched.size, "should not download anything before aborting"
    assert_includes @terminal.warnings.join, "1511"
  end

  def test_specific_revision
    rev2 = CollectionRevision.new(
      collection_id:   "n0nymh",
      collection_name: "cet-essentials",
      revision_number: 46,
      mods: [CollectionRevisionMod.new(mod_id: 107, file_id: 999)],
    )
    @provider.stub_revision("n0nymh", rev2)
    @provider.stub_manifest("n0nymh", [])
    stub_mod(@download, @archive, 107, 999, "nexus-107-999-cet-old", "0.9")

    interactor.call("n0nymh", revision: 46)
    assert @catalog.collection_exist?("nexus-n0nymh-cet-essentials-46")
  end

  private

  def stub_mod(download, archive, mod_id, file_id, slug, version, tmp_dir: nil)
    tmp_dir ||= Dir.mktmpdir("fake-mod-")
    unpacked = UnpackedMod.new(
      tmp_dir:,
      slug:,
      version:,
      game:    "cyberpunk2077",
      name:    slug,
      source:  { "provider" => "nexus", "mod_id" => mod_id, "file_id" => file_id },
    )
    download.stub_fetch(mod_id, file_id, unpacked_mod: unpacked)
  end
end

class ImportCollectionFomodTest < Minitest::Test
  def setup
    @provider = Adapters::CollectionProvider::Memory.new
    @download = Adapters::Download::Memory.new
    @archive  = Adapters::ModArchive::Memory.new
    @catalog  = Adapters::Catalog::Memory.new
    @terminal = Adapters::Terminal::Memory.new

    # Set up FOMOD mod tmpdir with fixture XML and both choice folders
    @fomod_dir = Dir.mktmpdir("fomod-mod-")
    FileUtils.mkdir_p(File.join(@fomod_dir, "fomod"))
    FileUtils.cp(FOMOD_FIXTURE_XML, File.join(@fomod_dir, "fomod", "ModuleConfig.xml"))
    FileUtils.mkdir_p(File.join(@fomod_dir, "Welcome to Night City", "bin"))
    FileUtils.mkdir_p(File.join(@fomod_dir, "Cyberpunk THING", "bin"))

    @rev = CollectionRevision.new(
      collection_id:   "iszwwe",
      collection_name: "welcome-to-night-city",
      revision_number: 481,
      mods: [
        CollectionRevisionMod.new(mod_id: 999, file_id: 100),   # normal mod
        CollectionRevisionMod.new(mod_id: 10426, file_id: 131647), # FOMOD mod
      ],
    )
    @provider.stub_revision("iszwwe", @rev)
    @provider.stub_manifest("iszwwe", [
      CollectionManifestMod.new(file_id: 100,    mod_id: 999,   name: "Normal Mod",   phase: 1, choices: nil),
      CollectionManifestMod.new(file_id: 131647, mod_id: 10426, name: "WTNC Config",  phase: 3, choices: nil),
    ])

    # Normal mod — plain tmpdir, no FOMOD
    stub_normal_mod(999, 100, "nexus-999-100-normal-mod-1-0", "1.0")

    # FOMOD mod — tmpdir with ModuleConfig.xml
    fomod_unpacked = UnpackedMod.new(
      tmp_dir: @fomod_dir,
      slug:    "nexus-10426-131647-wtnc-config-1-9-9-1",
      version: "1.9.9.1",
      game:    "cyberpunk2077",
      name:    "WTNC Config",
      source:  { "provider" => "nexus", "mod_id" => 10426, "file_id" => 131647 },
    )
    @download.stub_fetch(10426, 131647, unpacked_mod: fomod_unpacked)
  end

  def interactor
    Interactors::ImportCollection.new(
      provider: @provider, download: @download,
      archive: @archive, catalog: @catalog, terminal: @terminal,
      game: "cyberpunk2077",
    )
  end

  def test_creates_two_collections_for_fomod
    interactor.call("iszwwe")
    assert @catalog.collection_exist?("nexus-iszwwe-welcome-to-night-city-481"), "missing WTNC collection"
    assert @catalog.collection_exist?("nexus-iszwwe-cyberpunk-thing-481"),       "missing THING collection"
  end

  def test_each_collection_contains_shared_and_choice_slug
    interactor.call("iszwwe")
    wtnc  = @catalog.read_collection("nexus-iszwwe-welcome-to-night-city-481")
    thing = @catalog.read_collection("nexus-iszwwe-cyberpunk-thing-481")

    assert_includes wtnc.mods,  "nexus-999-100-normal-mod-1-0"
    assert_includes thing.mods, "nexus-999-100-normal-mod-1-0"

    assert(wtnc.mods.any?  { _1.include?("--welcome-to-night-city") }, "WTNC choice slug missing")
    assert(thing.mods.any? { _1.include?("--cyberpunk-thing") },       "THING choice slug missing")
  end

  def test_creates_two_modsets_for_fomod
    interactor.call("iszwwe")
    assert @catalog.modset_exist?("nexus-iszwwe-welcome-to-night-city-481")
    assert @catalog.modset_exist?("nexus-iszwwe-cyberpunk-thing-481")
  end

  private

  def stub_normal_mod(mod_id, file_id, slug, version)
    tmp = Dir.mktmpdir("fake-mod-")
    @download.stub_fetch(mod_id, file_id, unpacked_mod: UnpackedMod.new(
      tmp_dir: tmp, slug:, version:, game: "cyberpunk2077", name: slug,
      source: { "provider" => "nexus", "mod_id" => mod_id, "file_id" => file_id },
    ))
  end
end
