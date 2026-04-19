# frozen_string_literal: true

require "optparse"
require "mod_manager/deps"

module ModManager
  class CLI
    def self.run(argv, terminal = nil)
      terminal ||= Deps.terminal
      new(terminal).run(argv.dup)
    end

    def initialize(terminal)
      @terminal = terminal
    end

    def run(argv)
      catch(:exit) {
        dispatch(argv)
        0
      }
    rescue ValidationError => e
      e.errors.each { @terminal.error("  #{it}") }
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
        opts = {yes: false}
        OptionParser.new { |o| o.on("-y", "--yes") { opts[:yes] = true } }.parse!(argv)
        config = Deps.config
        Deps.reset_deploy(config:, terminal: @terminal).call(**opts)

      when "list"
        opts = {orphans_only: false}
        OptionParser.new { |o| o.on("-o", "--orphans") { opts[:orphans_only] = true } }.parse!(argv)
        config = Deps.config
        Deps.list_mods(config:, terminal: @terminal).call(**opts)

      when "status"
        config = Deps.config
        Deps.show_status(config:, terminal: @terminal).call

      when "verify"
        config = Deps.config
        ok = Deps.verify_catalog(config:, terminal: @terminal).call(names: [], mode: :all)
        throw :exit, 1 unless ok

      when "collection"
        action = argv.shift
        config = Deps.config
        crud = Deps.collection_crud(config:, terminal: @terminal)
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
          opts = {yes: false}
          OptionParser.new { |o| o.on("-y", "--yes") { opts[:yes] = true } }.parse!(argv)
          die("usage: mod collection delete [-y] <name>") if argv.empty?
          crud.delete(argv.first, **opts)
        when "verify"
          ok = Deps.verify_catalog(config:, terminal: @terminal).call(names: argv, mode: :collections)
          throw :exit, 1 unless ok

        when "import"
          opts = {provider: "nexus", revision: nil, list: false, info: false}
          OptionParser.new do |o|
            o.on("--provider=PROVIDER") { opts[:provider] = it }
            o.on("--revision=N", Integer) { opts[:revision] = it }
            o.on("--list") { opts[:list] = true }
            o.on("--info") { opts[:info] = true }
          end.parse!(argv)
          die("usage: mod collection import [--provider=nexus] [--revision=N] [--list|--info] <collection_id>") if argv.empty?
          cmd_import_collection(argv.first, **opts)
        else
          die("usage: mod collection <new|list|show|add|remove|delete|import|verify> ...")
        end

      when "cleanup"
        opts = {dry_run: false, yes: false}
        OptionParser.new do |o|
          o.on("-n", "--dry-run") { opts[:dry_run] = true }
          o.on("-y", "--yes") { opts[:yes] = true }
        end.parse!(argv)
        config = Deps.config
        Deps.cleanup(config:, terminal: @terminal).call(**opts)

      when "download"
        opts = {provider: nil, file_indices: nil, slug: nil, list: false, force: false}
        OptionParser.new do |o|
          o.on("--provider=PROVIDER") { opts[:provider] = it }
          o.on("--files=LIST") { opts[:file_indices] = it.split(",").map(&:to_i) }
          o.on("--slug=SLUG") { opts[:slug] = it }
          o.on("--list") { opts[:list] = true }
          o.on("-f", "--force") { opts[:force] = true }
        end.parse!(argv)
        die("usage: mod download [--provider=nexus] [--files=1,2] [--slug=SLUG] [--list] [-f] <mod_id>") if argv.empty?
        cmd_download(argv.first, **opts)

      when "modset"
        action = argv.shift
        config = Deps.config
        crud = Deps.modset_crud(config:, terminal: @terminal)
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
          opts = {yes: false}
          OptionParser.new { |o| o.on("-y", "--yes") { opts[:yes] = true } }.parse!(argv)
          die("usage: mod modset delete [-y] <name>") if argv.empty?
          crud.delete(argv.first, **opts)
        when "deploy"
          die("usage: mod modset deploy <name>") if argv.empty?
          Deps.deploy_modset(config:, terminal: @terminal).call(argv.first, raw_checks: Array(config.checks))
        when "verify"
          ok = Deps.verify_catalog(config:, terminal: @terminal).call(names: argv, mode: :modsets)
          throw :exit, 1 unless ok
        else
          die("usage: mod modset <new|list|show|add|remove|delete|deploy|verify> ...")
        end

      when "repair"
        opts = {mods: nil}
        OptionParser.new do |o|
          o.on("--mods=LIST") do |v|
            opts[:mods] = v.split(",").map { it.split(":").map(&:to_i) }
          end
        end.parse!(argv)
        config = Deps.config
        client = Deps.nexus_client
        Deps.repair_archive(config:, client:, terminal: @terminal).call(**opts)

      else
        die("usage: mod <list|reset|status|verify|collection|modset|cleanup|download|repair>")
      end
    end

    # ── commands ─────────────────────────────────────────────────────────────

    def cmd_import_collection(collection_id, provider:, revision:, list:, info:)
      case provider
      when "nexus"
        config = Deps.config
        client = Deps.nexus_client
        col_prov = Deps.collection_provider(config:, client:)
        if info
          archive = Deps.archive(config:)
          rev = col_prov.fetch_revision(slug: collection_id, revision:)
          manifest = col_prov.fetch_manifest(download_link: rev.download_link,
            slug: collection_id, revision: rev.revision_number)
          manifest_by_file_id = manifest.each_with_object({}) { |m, h| h[m.file_id] = m }
          col_base = "nexus-#{rev.collection_id}-#{rev.collection_name}-#{rev.revision_number}"

          fomod_entries = rev.mods.select do |m|
            mm = manifest_by_file_id[m.file_id]
            next false unless mm
            mm.choices.nil? && archive.all.none? { |a| a.source["mod_id"] == m.mod_id && a.source["file_id"] == m.file_id }
          end

          archived_set = archive.all.map { |a| [a.source["mod_id"], a.source["file_id"]] }.to_set

          width = rev.mods.filter_map { it.predicted_slug&.length }.max.to_i
          @terminal.info("#{col_base} (#{rev.mods.size} mods)\n")
          rev.mods.each do |m|
            slug = m.predicted_slug || "nexus-#{m.mod_id}-#{m.file_id}-?"
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
        Deps.import_collection(config:, client:, terminal: @terminal).call(collection_id, revision:)
      else
        raise Core::Error, "unknown provider: #{provider}"
      end
    end

    def cmd_download(mod_id, provider:, file_indices:, slug:, list:, force:)
      provider ||= detect_provider(mod_id)
      case provider
      when "nexus"
        config = Deps.config
        client = Deps.nexus_client
        download = Deps.download(config:, client:)
        sorted = download.list_files(mod_id: mod_id.to_i)

        if list
          Nexus::FilePicker.print_file_list(sorted)
          throw :exit, 0
        end

        files = if file_indices
          file_indices.map do |i|
            sorted[i - 1] or raise Core::Error, "--files #{i}: out of range (1-#{sorted.size})"
          end
        else
          file = Nexus::FilePicker.auto_select(sorted) or begin
            Nexus::FilePicker.print_file_list(sorted)
            throw :exit, 1
          end
          [file]
        end

        installer = Deps.install_mod(config:, client:, terminal: @terminal)
        files.each { installer.call(mod_id, file_id: it.file_id, slug:, force:) }
      else
        raise Core::Error, "unknown provider: #{provider}"
      end
    end

    def detect_provider(mod_id)
      return "nexus" if mod_id.match?(/\A\d+\z/)
      raise Core::Error, "cannot detect provider for #{mod_id.inspect} — use --provider=nexus"
    end
  end
end
