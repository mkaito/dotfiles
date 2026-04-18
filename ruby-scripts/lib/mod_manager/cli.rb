# frozen_string_literal: true

require "optparse"
require_relative "adapters/terminal/ansi"
require_relative "adapters/catalog/toml"
require_relative "adapters/mod_archive/filesystem"
require_relative "adapters/deploy/link_farm"
require_relative "interactors/deploy_modset"
require_relative "interactors/reset_deploy"
require_relative "interactors/show_status"
require_relative "interactors/validate"
require_relative "interactors/list_mods"
require_relative "interactors/cleanup"
require_relative "interactors/collection_crud"
require_relative "interactors/modset_crud"
require_relative "interactors/install_mod"
require_relative "interactors/import_collection"
require_relative "adapters/download/nexus"
require_relative "adapters/collection_provider/nexus"

module ModManager
  class CLI
    def self.run(argv, terminal = nil)
      terminal ||= Adapters::Terminal::Ansi.new
      new(terminal).run(argv.dup)
    end

    def initialize(terminal)
      @terminal = terminal
    end

    def run(argv)
      catch(:exit) { dispatch(argv); 0 }
    rescue ValidationError => e
      e.errors.each { @terminal.error("  #{_1}") }
      1
    rescue Error => e
      @terminal.error(e.message)
      1
    end

    private

    def die(msg)
      @terminal.error(msg)
      throw :exit, 1
    end

    # ── dispatch ──────────────────────────────────────────────────────────────

    def dispatch(argv)
      subcommand = argv.shift

      case subcommand
      when "reset"
        opts = { yes: false }
        OptionParser.new { |o| o.on("-y", "--yes") { opts[:yes] = true } }.parse!(argv)
        config, = load_context
        Interactors::ResetDeploy.new(
          deploy:   Adapters::Deploy::LinkFarm.new(config.game_dir, config.archive_dir),
          terminal: @terminal,
        ).call(**opts)

      when "list"
        opts = { orphans_only: false }
        OptionParser.new { |o| o.on("--orphans") { opts[:orphans_only] = true } }.parse!(argv)
        _, archive, catalog = load_context
        Interactors::ListMods.new(archive:, catalog:, terminal: @terminal).call(**opts)

      when "status"
        config, = load_context
        Interactors::ShowStatus.new(
          deploy:   Adapters::Deploy::LinkFarm.new(config.game_dir, config.archive_dir),
          terminal: @terminal,
        ).call

      when "validate"
        _, archive, catalog = load_context
        ok = Interactors::Validate.new(archive:, catalog:, terminal: @terminal).call(argv.first)
        throw :exit, 1 unless ok

      when "collection"
        action = argv.shift
        _, archive, catalog = load_context
        crud = Interactors::CollectionCrud.new(catalog:, archive:, terminal: @terminal)
        case action
        when "new"
          die("usage: mod collection new <name>") if argv.empty?
          crud.new_collection(argv.first)
        when "list"
          crud.list
        when "show"
          die("usage: mod collection show <name>") if argv.empty?
          crud.show(argv.first)
        when "add"
          die("usage: mod collection add <collection> <slug> [slug...]") if argv.size < 2
          crud.add(argv[0], argv[1..])
        when "remove"
          die("usage: mod collection remove <collection> <slug>") if argv.size < 2
          crud.remove(argv[0], argv[1])
        when "delete"
          opts = { yes: false }
          OptionParser.new { |o| o.on("-y", "--yes") { opts[:yes] = true } }.parse!(argv)
          die("usage: mod collection delete [-y] <name>") if argv.empty?
          crud.delete(argv.first, **opts)
        when "import"
          opts = { provider: "nexus", revision: nil, list: false, info: false }
          OptionParser.new do |o|
            o.on("--provider=PROVIDER")   { opts[:provider] = _1 }
            o.on("--revision=N", Integer) { opts[:revision] = _1 }
            o.on("--list")                { opts[:list] = true }
            o.on("--info")                { opts[:info] = true }
          end.parse!(argv)
          die("usage: mod collection import [--provider=nexus] [--revision=N] [--list|--info] <collection_id>") if argv.empty?
          cmd_import_collection(argv.first, **opts)
        else
          die("usage: mod collection <new|list|show|add|remove|delete|import> ...")
        end

      when "cleanup"
        opts = { dry_run: false, yes: false }
        OptionParser.new do |o|
          o.on("-n", "--dry-run") { opts[:dry_run] = true }
          o.on("-y", "--yes")     { opts[:yes] = true }
        end.parse!(argv)
        _, archive, catalog = load_context
        Interactors::Cleanup.new(archive:, catalog:, terminal: @terminal).call(**opts)

      when "download"
        opts = { provider: nil, file_index: nil, slug: nil, list: false }
        OptionParser.new do |o|
          o.on("--provider=PROVIDER") { opts[:provider] = _1 }
          o.on("--file=N", Integer)   { opts[:file_index] = _1 }
          o.on("--slug=SLUG")         { opts[:slug] = _1 }
          o.on("--list")              { opts[:list] = true }
        end.parse!(argv)
        die("usage: mod download [--provider=nexus] [--file=N] [--slug=SLUG] [--list] <mod_id>") if argv.empty?
        cmd_download(argv.first, **opts)

      when "modset"
        action = argv.shift
        config, _, catalog = load_context
        crud = Interactors::ModsetCrud.new(catalog:, terminal: @terminal, game: config.domain, modsets_dir: config.modsets_dir)
        case action
        when "new"
          die("usage: mod modset new <name>") if argv.empty?
          crud.new_modset(argv.first)
        when "list"
          crud.list
        when "show"
          die("usage: mod modset show <name>") if argv.empty?
          crud.show(argv.first)
        when "add"
          die("usage: mod modset add <modset> <collection> [collection...]") if argv.size < 2
          crud.add(argv[0], argv[1..])
        when "remove"
          die("usage: mod modset remove <modset> <collection>") if argv.size < 2
          crud.remove(argv[0], argv[1])
        when "delete"
          opts = { yes: false }
          OptionParser.new { |o| o.on("-y", "--yes") { opts[:yes] = true } }.parse!(argv)
          die("usage: mod modset delete [-y] <name>") if argv.empty?
          crud.delete(argv.first, **opts)
        when "deploy"
          die("usage: mod modset deploy <name>") if argv.empty?
          archive = Adapters::ModArchive::Filesystem.new(config.archive_dir)
          Interactors::DeployModset.new(
            catalog:  catalog,
            archive:  archive,
            deploy:   Adapters::Deploy::LinkFarm.new(config.game_dir, config.archive_dir),
            terminal: @terminal,
          ).call(argv.first, raw_checks: Array(config.checks))
        else
          die("usage: mod modset <new|list|show|add|remove|delete|deploy> ...")
        end

      else
        die("usage: mod <list|reset|status|validate|collection|modset|cleanup|download>")
      end
    end

    # ── helpers ──────────────────────────────────────────────────────────────

    def load_context
      config  = Config.load
      archive = Adapters::ModArchive::Filesystem.new(config.archive_dir)
      catalog = Adapters::Catalog::Toml.new(config.collections_dir, config.modsets_dir)
      [config, archive, catalog]
    end

    def nexus_client
      key = ENV["NEXUS_API_KEY"] or raise Core::Error, "NEXUS_API_KEY not set (set it in mise.local.toml or export it)"
      Nexus::Client.new(key)
    end

    def detect_provider(mod_id)
      return "nexus" if mod_id.match?(/\A\d+\z/)
      raise Core::Error, "cannot detect provider for #{mod_id.inspect} — use --provider=nexus"
    end

    # ── commands ─────────────────────────────────────────────────────────────

    def cmd_import_collection(collection_id, provider:, revision:, list:, info:)
      case provider
      when "nexus"
        client              = nexus_client
        config, archive, catalog = load_context
        col_prov            = Adapters::CollectionProvider::Nexus.new(config.domain, client)
        download            = Adapters::Download::Nexus.new(config.domain, client)
        if info
          rev      = col_prov.fetch_revision(slug: collection_id, revision:)
          manifest = col_prov.fetch_manifest(download_link: rev.download_link,
                                             slug: collection_id, revision: rev.revision_number)
          manifest_by_file_id = manifest.each_with_object({}) { |m, h| h[m.file_id] = m }
          col_base = "nexus-#{rev.collection_id}-#{rev.collection_name}-#{rev.revision_number}"

          # Detect unresolved FOMOD mods (choices: nil in manifest, or missing from manifest)
          fomod_entries = rev.mods.select do |m|
            mm = manifest_by_file_id[m.file_id]
            next false unless mm
            mm.choices.nil? && archive.all.none? { |a| a.source["mod_id"] == m.mod_id && a.source["file_id"] == m.file_id }
          end

          archived_set = archive.all.map { |a| [a.source["mod_id"], a.source["file_id"]] }.to_set

          width = rev.mods.filter_map { _1.predicted_slug&.length }.max.to_i
          @terminal.info("#{col_base} (#{rev.mods.size} mods)\n")
          rev.mods.each do |m|
            slug   = m.predicted_slug || "nexus-#{m.mod_id}-#{m.file_id}-?"
            cached = archived_set.include?([m.mod_id, m.file_id])
            status = cached ? "[archived]" : "[missing]"
            @terminal.info("  #{slug.ljust(width)}  #{@terminal.muted(status)}")
          end

          if fomod_entries.any?
            @terminal.info("\n  FOMOD detected — import will create separate collections per choice")
            @terminal.info("  (choices unknown until mod is downloaded; run import to resolve)")
          end

          throw :exit, 0
        end
        if list
          summaries = col_prov.list_revisions(slug: collection_id)
          if summaries.empty?
            @terminal.info("no revisions found for #{collection_id}")
          else
            summaries.each do |s|
              @terminal.info("  #{@terminal.bold(s.revision_number.to_s)}  #{s.status}  #{s.mod_count} mods  #{s.created_at}")
            end
          end
          throw :exit, 0
        end
        Interactors::ImportCollection.new(
          provider: col_prov, download:, archive:, catalog:, terminal: @terminal,
          game: config.domain,
        ).call(collection_id, revision:)
      else
        raise Core::Error, "unknown provider: #{provider}"
      end
    end

    def cmd_download(mod_id, provider:, file_index:, slug:, list:)
      provider ||= detect_provider(mod_id)

      case provider
      when "nexus"
        client   = nexus_client
        config, archive = load_context
        download = Adapters::Download::Nexus.new(config.domain, client)
        sorted   = download.list_files(mod_id:)

        file = if list
          Nexus::FilePicker.print_file_list(sorted)
          throw :exit, 0
        elsif file_index
          sorted[file_index - 1] or raise Core::Error, "--file #{file_index}: out of range (1-#{sorted.size})"
        else
          Nexus::FilePicker.auto_select(sorted) or begin
            Nexus::FilePicker.print_file_list(sorted)
            throw :exit, 1
          end
        end

        Interactors::InstallMod.new(download:, archive:, terminal: @terminal)
          .call(mod_id, file_id: file["file_id"], slug:)
      else
        raise Core::Error, "unknown provider: #{provider}"
      end
    end
  end
end
