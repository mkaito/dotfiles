# frozen_string_literal: true

require "optparse"
require "fileutils"
require "set"

module ModManager
  module CLI
    def self.run(argv, out = $stdout)
      old, $stdout = $stdout, out
      catch(:exit) { dispatch(argv.dup); 0 }
    ensure
      $stdout = old
    end

    def self.die(msg)
      $stdout.puts msg
      throw :exit, 1
    end

    def self.dispatch(argv)
      subcommand = argv.shift

      case subcommand
      when "reset"
        opts = { yes: false }
        OptionParser.new { |o| o.on("-y", "--yes") { opts[:yes] = true } }.parse!(argv)
        cmd_reset(**opts)

      when "list"
        opts = { orphans_only: false }
        OptionParser.new { |o| o.on("--orphans") { opts[:orphans_only] = true } }.parse!(argv)
        cmd_list(**opts)

      when "status"
        cmd_status

      when "validate"
        cmd_validate(argv.first)

      when "collection"
        action = argv.shift
        case action
        when "new"
          die("usage: mod collection new <name>") if argv.empty?
          cmd_collection_new(argv.first)
        when "list"
          cmd_collection_list
        when "show"
          die("usage: mod collection show <name>") if argv.empty?
          cmd_collection_show(argv.first)
        when "add"
          die("usage: mod collection add <collection> <slug> [slug...]") if argv.size < 2
          cmd_collection_add(argv[0], argv[1..])
        when "remove"
          die("usage: mod collection remove <collection> <slug>") if argv.size < 2
          cmd_collection_remove(argv[0], argv[1])
        when "delete"
          opts = { yes: false }
          OptionParser.new { |o| o.on("-y", "--yes") { opts[:yes] = true } }.parse!(argv)
          die("usage: mod collection delete [-y] <name>") if argv.empty?
          cmd_collection_delete(argv.first, **opts)
        else
          die("usage: mod collection <new|list|show|add|remove|delete> ...")
        end

      when "cleanup"
        opts = { dry_run: false, yes: false }
        OptionParser.new do |o|
          o.on("-n", "--dry-run") { opts[:dry_run] = true }
          o.on("-y", "--yes")     { opts[:yes] = true }
        end.parse!(argv)
        cmd_cleanup(**opts)

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
        case action
        when "new"
          die("usage: mod modset new <name>") if argv.empty?
          cmd_modset_new(argv.first)
        when "list"
          cmd_modset_list
        when "show"
          die("usage: mod modset show <name>") if argv.empty?
          cmd_modset_show(argv.first)
        when "add"
          die("usage: mod modset add <modset> <collection> [collection...]") if argv.size < 2
          cmd_modset_add(argv[0], argv[1..])
        when "remove"
          die("usage: mod modset remove <modset> <collection>") if argv.size < 2
          cmd_modset_remove(argv[0], argv[1])
        when "delete"
          opts = { yes: false }
          OptionParser.new { |o| o.on("-y", "--yes") { opts[:yes] = true } }.parse!(argv)
          die("usage: mod modset delete [-y] <name>") if argv.empty?
          cmd_modset_delete(argv.first, **opts)
        when "deploy"
          die("usage: mod modset deploy <name>") if argv.empty?
          cmd_deploy(argv.first)
        else
          die("usage: mod modset <new|list|show|add|remove|delete|deploy> ...")
        end

      else
        die("usage: mod <list|reset|status|validate|collection|modset|cleanup|download>")
      end
    rescue ValidationError => e
      e.errors.each { puts "  #{_1}" }
      throw :exit, 1
    rescue Error => e
      $stdout.puts e.message
      throw :exit, 1
    end

    # ── helpers ──────────────────────────────────────────────────────────────

    def self.load_context
      config  = Config.load
      archive = Archive.new(config.archive_dir)
      [config, archive]
    end

    def self.chezmoi_add(path)
      system("chezmoi", "add", path, out: File::NULL, err: File::NULL)
    end

    def self.nexus_env_file
      File.join(Core::XDG.config_home, "mods", "nexus.env")
    end

    def self.nexus_client
      key = ENV["NEXUS_API_KEY"] or raise Core::Error, <<~MSG.chomp
        NEXUS_API_KEY not set. Options:
          1. op run --env-file=#{nexus_env_file} -- mod download ...
             (create #{nexus_env_file} with: NEXUS_API_KEY=op://Private/Nexusmods/add more/API Key)
          2. NEXUS_API_KEY="op://Private/Nexusmods/add more/API Key" op run -- mod download ...
      MSG
      Nexus::Client.new(key)
    end

    def self.detect_provider(mod_id)
      return "nexus" if mod_id.match?(/\A\d+\z/)
      raise Core::Error, "cannot detect provider for #{mod_id.inspect} — use --provider=nexus"
    end

    # ── commands ─────────────────────────────────────────────────────────────

    def self.cmd_download(mod_id, provider:, file_index:, slug:, list:)
      config, archive = load_context
      provider ||= detect_provider(mod_id)

      case provider
      when "nexus"
        client      = nexus_client
        game_domain = config.domain
        files       = client.mod_files(game_domain, mod_id.to_i)
        sorted      = Nexus::Installer.sort_files(files)

        file = if list
          Nexus::Installer.print_file_list(sorted)
          throw :exit, 0
        elsif file_index
          sorted[file_index - 1] or raise Core::Error, "--file #{file_index}: out of range (1-#{sorted.size})"
        else
          Nexus::Installer.auto_select(sorted) or begin
            Nexus::Installer.print_file_list(sorted)
            throw :exit, 1
          end
        end

        installed_slug, _version = Nexus::Installer.install(
          client, game_domain, mod_id.to_i,
          archive_dir: config.archive_dir, file_id: file["file_id"], slug: slug
        )
        archive.invalidate
        puts "installed #{installed_slug}"
      else
        raise Core::Error, "unknown provider: #{provider}"
      end
    end

    def self.cmd_deploy(modset_name)
      config, archive = load_context
      path = File.join(config.modsets_dir, "#{modset_name}.toml")
      raise Error, "modset not found: #{path}" unless File.exist?(path)
      ms = Modset.load(path)

      check_errors = Checker.new(archive, config).check_modset(ms)
      if check_errors.any?
        check_errors.each { puts "  #{Core::Format.red(_1)}" }
        throw :exit, 1
      end

      mods, conflicts = ms.resolve(archive, config)
      raise Error, "modset has no mods to deploy" if mods.empty?

      conflicts.each do |key, slugs|
        loser, winner = slugs.uniq.first, slugs.uniq.last
        puts Core::Format.yellow("conflict: #{key} — #{loser} overridden by #{winner}")
      end

      result = Deployer.new(config.game_dir, config.archive_dir).deploy(mods)
      puts "deployed #{result[:created]} file(s) from #{mods.size} mod(s)"

      vfails = Verifier.run(Verifier.collect(config), config.game_dir)
      if vfails.any?
        puts "verify warnings (#{vfails.size}):"
        vfails.each { puts "  #{_1}" }
      end
    end

    def self.cmd_reset(yes:)
      config, = load_context
      deployer = Deployer.new(config.game_dir, config.archive_dir)
      st    = deployer.status
      total = st.values.sum { _1[:links].size + _1[:broken].size }

      if total.zero?
        puts "no active symlinks"
        return
      end

      puts "#{total} symlink(s) to remove"
      unless yes || Core::CLI.confirm("proceed?")
        puts "aborted"
        return
      end

      puts "removed #{deployer.undeploy} symlink(s)"
    end

    def self.cmd_status
      config, = load_context
      st = Deployer.new(config.game_dir, config.archive_dir).status

      if st.empty?
        puts Core::Format.dim("no active mods")
        return
      end

      st.each do |slug_ver, data|
        suffix = data[:broken].any? ? ", #{Core::Format.red("#{data[:broken].size} broken")}" : ""
        puts "#{slug_ver}: #{data[:links].size} link(s)#{suffix}"
        data[:broken].each { puts "  #{Core::Format.red("broken:")} #{_1}" }
      end
    end

    def self.cmd_validate(name = nil)
      config, archive = load_context
      checker = Checker.new(archive, config)

      errors = if name
        path = File.join(config.modsets_dir, "#{name}.toml")
        if File.exist?(path)
          checker.check_modset(Modset.load(path))
        else
          path = File.join(config.collections_dir, "#{name}.toml")
          raise Error, "no modset or collection named #{name.inspect}" unless File.exist?(path)
          checker.check_collection(Collection.load(path))
        end
      else
        checker.check_all
      end

      if errors.empty?
        puts Core::Format.green("ok")
      else
        errors.each { puts "  #{Core::Format.red(_1)}" }
        throw :exit, 1
      end
    end

    def self.cmd_list(orphans_only:)
      config, archive = load_context
      if archive.all.empty?
        puts Core::Format.dim("archive empty")
        return
      end

      membership = Hash.new { |h, k| h[k] = [] }
      Dir.glob("#{config.collections_dir}/*.toml").each do |p|
        col_name = File.basename(p, ".toml")
        Collection.load(p).mods.each { membership[_1] << col_name }
      rescue ValidationError
        # skip invalid collections silently
      end

      mods = archive.all.sort_by(&:slug)
      mods = mods.reject { membership[_1.slug].any? } if orphans_only

      if mods.empty?
        puts Core::Format.dim("no uncollected mods")
        return
      end

      width = mods.map { _1.slug.length }.max
      mods.each do |mod|
        cols = membership[mod.slug]
        if cols.any?
          puts "#{mod.slug.ljust(width)}  #{Core::Format.dim("[#{cols.join(", ")}]")}"
        else
          puts mod.slug
        end
      end
    end

    def self.cmd_collection_add(collection_name, slugs)
      config, archive = load_context
      path = File.join(config.collections_dir, "#{collection_name}.toml")
      raise Error, "collection not found: #{path}" unless File.exist?(path)
      CollectionEditor.add(path, slugs, archive)
      chezmoi_add(path)
      puts "updated #{collection_name}"
    end

    def self.cmd_collection_remove(collection_name, slug)
      config, = load_context
      path = File.join(config.collections_dir, "#{collection_name}.toml")
      raise Error, "collection not found: #{path}" unless File.exist?(path)
      CollectionEditor.remove(path, slug)
      chezmoi_add(path)
      puts "updated #{collection_name}"
    end

    def self.cmd_collection_new(name)
      config, = load_context
      path = File.join(config.collections_dir, "#{name}.toml")
      raise Error, "collection already exists: #{path}" if File.exist?(path)
      Core::FileIO.atomic_write(path, TomlRB.dump("name" => name, "mods" => []))
      chezmoi_add(path)
      puts "created #{name}"
    end

    def self.cmd_collection_list
      config, = load_context
      paths = Dir.glob("#{config.collections_dir}/*.toml").sort
      if paths.empty?
        puts Core::Format.dim("no collections")
        return
      end
      paths.each do |p|
        col = Collection.load(p)
        puts "#{Core::Format.bold(File.basename(p, ".toml"))}: #{col.mods.size} mod(s)"
      rescue ValidationError => e
        puts "#{Core::Format.bold(File.basename(p, ".toml"))}: #{Core::Format.red("(invalid: #{e.errors.first})")}"
      end
    end

    def self.cmd_collection_show(name)
      config, = load_context
      path = File.join(config.collections_dir, "#{name}.toml")
      raise Error, "collection not found: #{path}" unless File.exist?(path)
      col = Collection.load(path)
      if col.mods.empty?
        puts "(empty)"
      else
        col.mods.each { puts _1 }
      end
    end

    def self.cmd_collection_delete(name, yes:)
      config, = load_context
      path = File.join(config.collections_dir, "#{name}.toml")
      raise Error, "collection not found: #{path}" unless File.exist?(path)
      unless yes || Core::CLI.confirm("delete #{name}?")
        puts "aborted"
        return
      end
      File.delete(path)
      puts "deleted #{name}"
    end

    def self.cmd_cleanup(dry_run:, yes:)
      config, archive = load_context
      referenced = Set.new

      Dir.glob("#{config.collections_dir}/*.toml").each do |p|
        Collection.load(p).mods.each { referenced << _1 }
      rescue ValidationError => e
        e.errors.each { puts Core::Format.yellow("warning: #{_1}") }
      end
      Dir.glob("#{config.modsets_dir}/*.toml").each do |p|
        Modset.load(p).mods.each { referenced << _1 }
      rescue ValidationError => e
        e.errors.each { puts Core::Format.yellow("warning: #{_1}") }
      end

      orphans = archive.all.reject { referenced.include?(_1.slug) }

      if orphans.empty?
        puts "no orphaned mods"
        return
      end

      puts "orphaned (#{orphans.size}):"
      orphans.each { puts "  #{_1}" }
      return if dry_run

      unless yes || Core::CLI.confirm("delete #{orphans.size} mod(s)?")
        puts "aborted"
        return
      end

      orphans.each do |mod|
        FileUtils.rm_rf(mod.path)
        parent = File.dirname(mod.path)
        Dir.rmdir(parent) if Dir.empty?(parent)
        Log.info("deleted #{mod}")
      end
      puts "deleted #{orphans.size} mod(s)"
    end

    def self.cmd_modset_new(name)
      config, = load_context
      path = File.join(config.modsets_dir, "#{name}.toml")
      raise Error, "modset already exists: #{path}" if File.exist?(path)
      Core::FileIO.atomic_write(path, TomlRB.dump("game" => config.domain, "collections" => [], "mods" => []))
      chezmoi_add(path)
      puts "created #{name}"
    end

    def self.cmd_modset_list
      config, = load_context
      paths = Dir.glob("#{config.modsets_dir}/*.toml").sort
      if paths.empty?
        puts Core::Format.dim("no modsets")
        return
      end
      paths.each do |p|
        ms = Modset.load(p)
        puts "#{Core::Format.bold(File.basename(p, ".toml"))}: #{ms.collections.size} collection(s)"
      rescue ValidationError => e
        puts "#{Core::Format.bold(File.basename(p, ".toml"))}: #{Core::Format.red("(invalid: #{e.errors.first})")}"
      end
    end

    def self.cmd_modset_show(name)
      config, = load_context
      path = File.join(config.modsets_dir, "#{name}.toml")
      raise Error, "modset not found: #{path}" unless File.exist?(path)
      ms = Modset.load(path)
      if ms.collections.empty? && ms.mods.empty?
        puts "(empty)"
      else
        ms.collections.each { puts _1 }
        ms.mods.each { puts "  #{_1} (direct mod)" } if ms.mods.any?
      end
    end

    def self.cmd_modset_add(name, collection_names)
      config, = load_context
      path = File.join(config.modsets_dir, "#{name}.toml")
      raise Error, "modset not found: #{path}" unless File.exist?(path)
      ModsetEditor.add(path, collection_names, config)
      chezmoi_add(path)
      puts "updated #{name}"
    end

    def self.cmd_modset_remove(name, collection_name)
      config, = load_context
      path = File.join(config.modsets_dir, "#{name}.toml")
      raise Error, "modset not found: #{path}" unless File.exist?(path)
      ModsetEditor.remove(path, collection_name)
      chezmoi_add(path)
      puts "updated #{name}"
    end

    def self.cmd_modset_delete(name, yes:)
      config, = load_context
      path = File.join(config.modsets_dir, "#{name}.toml")
      raise Error, "modset not found: #{path}" unless File.exist?(path)
      unless yes || Core::CLI.confirm("delete #{name}?")
        puts "aborted"
        return
      end
      File.delete(path)
      puts "deleted #{name}"
    end
  end
end
