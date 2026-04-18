# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/interactors/deploy_modset"
require "mod_manager/adapters/catalog/memory"
require "mod_manager/adapters/mod_archive/memory"
require "mod_manager/adapters/deploy/memory"
require "mod_manager/adapters/terminal/memory"

include ModManager

class DeployModsetIntegrationTest < Minitest::Test
  def setup
    @catalog  = Adapters::Catalog::Memory.new
    @archive  = Adapters::ModArchive::Memory.new
    @deploy   = Adapters::Deploy::Memory.new
    @terminal = Adapters::Terminal::Memory.new

    @archive.seed("redscript",  game: "cp2077")
    @archive.seed("cyber-engine-tweaks", game: "cp2077")
    @catalog.seed_collection("core", mods: %w[redscript cyber-engine-tweaks])
    @catalog.seed_modset("main", collections: %w[core])
  end

  def interactor
    Interactors::DeployModset.new(catalog: @catalog, archive: @archive, deploy: @deploy, terminal: @terminal)
  end

  def test_deploys_mods_from_collections
    interactor.call("main")
    assert_match(/deployed/, @terminal.output)
  end

  def test_raises_on_missing_modset
    assert_raises(Error) { interactor.call("no-such-modset") }
  end

  def test_raises_validation_error_when_slug_not_in_archive
    @catalog.seed_collection("bad", mods: %w[missing-mod])
    @catalog.seed_modset("bad-set", collections: %w[bad])
    assert_raises(ValidationError) { interactor.call("bad-set") }
  end

  def test_raises_when_modset_has_no_mods
    @catalog.seed_collection("empty", mods: [])
    @catalog.seed_modset("empty-set", collections: %w[empty])
    assert_raises(Error) { interactor.call("empty-set") }
  end
end
