# frozen_string_literal: true

require "mod_manager/config"
require "mod_manager/adapters/terminal/ansi"
require "mod_manager/adapters/catalog/toml"
require "mod_manager/adapters/mod_archive/filesystem"
require "mod_manager/adapters/deploy/link_farm"
require "mod_manager/adapters/download/nexus"
require "mod_manager/adapters/collection_provider/nexus"
require "mod_manager/interactors/deploy_modset"
require "mod_manager/interactors/reset_deploy"
require "mod_manager/interactors/show_status"
require "mod_manager/interactors/validate"
require "mod_manager/interactors/list_mods"
require "mod_manager/interactors/cleanup"
require "mod_manager/interactors/collection_crud"
require "mod_manager/interactors/modset_crud"
require "mod_manager/interactors/install_mod"
require "mod_manager/interactors/import_collection"
require "mod_manager/interactors/repair_archive"
require "mod_manager/interactors/verify_catalog"
require "nexus/client"
require "nexus/file_picker"
require "core/errors"

module ModManager
  module Deps
    def self.config    = Config.load
    def self.terminal  = Adapters::Terminal::Ansi.new

    def self.nexus_client
      key = ENV["NEXUS_API_KEY"] or raise Core::Error, "NEXUS_API_KEY not set (set it in mise.local.toml or export it)"
      Nexus::Client.new(key)
    end

    def self.archive(config:)                      = Adapters::ModArchive::Filesystem.new(config.archive_dir)
    def self.catalog(config:)                      = Adapters::Catalog::Toml.new(config.collections_dir, config.modsets_dir)
    def self.deploy(config:)                       = Adapters::Deploy::LinkFarm.new(config.game_dir, config.archive_dir)
    def self.download(config:, client:)            = Adapters::Download::Nexus.new(config.domain, client)
    def self.collection_provider(config:, client:) = Adapters::CollectionProvider::Nexus.new(config.domain, client)

    def self.deploy_modset(config:, terminal:)
      Interactors::DeployModset.new(
        catalog: catalog(config:), archive: archive(config:),
        deploy: deploy(config:), terminal:,
      )
    end

    def self.reset_deploy(config:, terminal:)
      Interactors::ResetDeploy.new(deploy: deploy(config:), terminal:)
    end

    def self.show_status(config:, terminal:)
      Interactors::ShowStatus.new(deploy: deploy(config:), terminal:)
    end

    def self.validate(config:, terminal:)
      Interactors::Validate.new(catalog: catalog(config:), archive: archive(config:), terminal:)
    end

    def self.verify_catalog(config:, terminal:)
      Interactors::VerifyCatalog.new(catalog: catalog(config:), archive: archive(config:), terminal:)
    end

    def self.list_mods(config:, terminal:)
      Interactors::ListMods.new(archive: archive(config:), catalog: catalog(config:), terminal:)
    end

    def self.cleanup(config:, terminal:)
      Interactors::Cleanup.new(archive: archive(config:), catalog: catalog(config:), terminal:)
    end

    def self.collection_crud(config:, terminal:)
      Interactors::CollectionCrud.new(catalog: catalog(config:), archive: archive(config:), terminal:)
    end

    def self.modset_crud(config:, terminal:)
      Interactors::ModsetCrud.new(
        catalog: catalog(config:), terminal:,
        game: config.domain, modsets_dir: config.modsets_dir,
      )
    end

    def self.install_mod(config:, client:, terminal:)
      Interactors::InstallMod.new(
        download: download(config:, client:), archive: archive(config:), terminal:,
      )
    end

    def self.repair_archive(config:, client:, terminal:)
      Interactors::RepairArchive.new(
        archive: archive(config:), download: download(config:, client:), terminal:,
      )
    end

    def self.import_collection(config:, client:, terminal:)
      Interactors::ImportCollection.new(
        provider: collection_provider(config:, client:),
        download: download(config:, client:),
        archive: archive(config:), catalog: catalog(config:),
        terminal:, game: config.domain,
      )
    end
  end
end
