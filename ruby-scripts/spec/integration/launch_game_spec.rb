# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/interactors/launch_game"
require "mod_manager/adapters/catalog/memory"
require "mod_manager/adapters/mod_archive/memory"
require "mod_manager/adapters/deploy/memory"
require "mod_manager/adapters/runtime_launch/memory"
require "mod_manager/adapters/terminal/memory"

include ModManager

class LaunchGameIntegrationTest < Minitest::Test
  def setup
    @catalog  = Adapters::Catalog::Memory.new
    @archive  = Adapters::ModArchive::Memory.new
    @deploy   = Adapters::Deploy::Memory.new
    @launch   = Adapters::RuntimeLaunch::Memory.new
    @terminal = Adapters::Terminal::Memory.new

    @archive.seed("cet",       game: "cp2077")
    @archive.seed("redscript", game: "cp2077")
    @catalog.seed_collection("core", mods: %w[cet redscript])
    @catalog.seed_modset("main", collections: %w[core])
  end

  def interactor
    Interactors::LaunchGame.new(
      catalog: @catalog, archive: @archive,
      deploy: @deploy, runtime_launch: @launch, terminal: @terminal
    )
  end

  def test_resolves_mods_and_launches
    interactor.call("main", command: ["echo", "ok"])
    assert_equal 1, @launch.launches.size
    assert_equal ["echo", "ok"], @launch.launches.first[:command]
    assert_equal "main", @launch.launches.first[:modset]
    assert_equal 2, @launch.launches.first[:mods].size
  end

  def test_raises_validation_error_when_mod_missing_from_archive
    @catalog.seed_collection("bad", mods: %w[missing])
    @catalog.seed_modset("bad-set", collections: %w[bad])
    assert_raises(ValidationError) { interactor.call("bad-set", command: ["echo"]) }
  end

  def test_raises_when_modset_has_no_mods
    @catalog.seed_collection("empty", mods: [])
    @catalog.seed_modset("empty-set", collections: %w[empty])
    assert_raises(Error) { interactor.call("empty-set", command: ["echo"]) }
  end

  def test_raises_on_missing_modset
    assert_raises(Error) { interactor.call("no-such-modset", command: ["echo"]) }
  end

  def test_warns_when_link_farm_is_deployed
    @deploy.links["cet/1.0/bin/x64/global.ini"] = "/archive/cet/1.0/bin/x64/global.ini"
    interactor.call("main", command: ["echo"])
    assert_match(/link farm deployed/, @terminal.output)
  end

  def test_no_warning_when_link_farm_is_clean
    interactor.call("main", command: ["echo"])
    refute_match(/link farm deployed/, @terminal.output)
  end

  def test_warns_on_conflicts
    # Two slugs with the same upstream mod_id resolve to the same key → conflict
    @archive.seed("cet-old", game: "cp2077", source: {"provider" => "nexus", "mod_id" => 107})
    @archive.seed("cet-new", game: "cp2077", source: {"provider" => "nexus", "mod_id" => 107})
    @catalog.seed_collection("col-old", mods: %w[cet-old])
    @catalog.seed_collection("col-new", mods: %w[cet-new])
    @catalog.seed_modset("conflict-set", collections: %w[col-old col-new])
    interactor.call("conflict-set", command: ["echo"])
    assert_match(/conflict/, @terminal.output)
  end

  def test_logs_launch_and_exit
    interactor.call("main", command: ["echo"])
    assert_match(/launching main/, @terminal.output)
    assert_match(/exited/, @terminal.output)
  end
end
