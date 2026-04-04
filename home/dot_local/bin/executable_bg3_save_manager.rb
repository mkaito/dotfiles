#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"
require "time"
require "fileutils"
require "open3"

module BG3
  Error = Class.new(StandardError)
  SaveNotFoundError = Class.new(Error)
  ArchiveError = Class.new(Error)

  Config = Struct.new(:steam_base, :backup_dir, :keep_count, keyword_init: true) do
    def self.from_env
      new(
        steam_base: Pathname(ENV.fetch("STEAM_BASE", "/mnt/steamone/SteamLibrary")),
        backup_dir: Pathname(ENV.fetch("BG3_BACKUP_DIR", "#{ENV.fetch("HOME")}/Documents/BG3_Honour_Backups")).expand_path,
        keep_count: Integer(ENV.fetch("KEEP_BACKUPS", "5"))
      )
    end

    def bg3_base
      @bg3_base ||= steam_base.join(
        "steamapps", "compatdata", "1086940", "pfx", "drive_c", "users", "steamuser",
        "AppData", "Local", "Larian Studios", "Baldur's Gate 3"
      )
    end

    def save_dirs
      @save_dirs ||= [
        bg3_base.join("PlayerProfiles", "Public", "Savegames", "Story"),
        bg3_base.join("Story")
      ].select(&:directory?)
    end

    def ensure_backup_dir!
      backup_dir.mkpath
    end
  end

  module TTY
    C = { green: 32, red: 31, yellow: 33, blue: 34, cyan: 36 }.freeze
    module_function
    def colour(s, c)
      enabled = $stdout.tty? && ENV["NO_COLOR"].nil? && ENV["TERM"] != "dumb"
      enabled ? "\e[0;#{C[c]}m#{s}\e[0m" : s
    end
    def green(s)  = colour(s, :green)
    def red(s)    = colour(s, :red)
    def yellow(s) = colour(s, :yellow)
    def blue(s)   = colour(s, :blue)
    def cyan(s)   = colour(s, :cyan)
    def confirm?(q)
      return true if ENV["BG3_YES"] == "1"
      print yellow("#{q} (y/N): ")
      ans = $stdin.gets.to_s.strip.downcase
      ans == "y" || ans == "yes"
    end
  end

  class Save
    attr_reader :path, :folder_name, :uuid
    def initialize(path)
      @path = Pathname(path)
      @folder_name = @path.basename.to_s
      @uuid = folder_name[/^([a-f0-9-]+)__HonourMode$/i, 1] || folder_name
    end
    def honour_mode?
      folder_name.include?("__HonourMode") || lsv_files.any? { _1.basename.to_s.include?("HonourMode") }
    end
    def lsv_files = @lsv_files ||= path.glob("*.lsv").select(&:file?)
    def timestamp = @timestamp ||= (lsv_files.max_by(&:mtime)&.mtime || Time.at(0))
    def timestamp_s = timestamp == Time.at(0) ? "Unknown" : timestamp.strftime("%Y-%m-%d %H:%M:%S")
    def size_mb
      @size_mb ||= begin
        total = 0
        path.glob("**/*").each { |f| total += f.size if f.file? }
        total / (1024.0 * 1024.0)
      rescue
        0.0
      end
    end
    def short = uuid[0, 8]
  end

  class Archive
    EXT = ".tar.xz"
    PREFIX = "__HonourMode_"
    RGX = /^([a-f0-9-]+)#{Regexp.escape(PREFIX)}(\d{4}-\d{2}-\d{2}-\d{2}-\d{2})(?:_(.+))?#{Regexp.escape(EXT)}$/i

    attr_reader :save_uuid, :timestamp, :custom_name, :path
    def initialize(save_uuid:, timestamp:, custom_name: nil, path:)
      @save_uuid, @timestamp, @custom_name, @path = save_uuid, timestamp, custom_name, Pathname(path)
    end

    def self.from_save(save, backup_dir:, name: nil)
      new(
        save_uuid: save.uuid,
        timestamp: save.timestamp,
        custom_name: name,
        path: backup_dir / filename_for(save.uuid, save.timestamp, name)
      )
    end

    def self.from_path(path)
      pn = Pathname(path)
      m = pn.basename.to_s.match(RGX)
      unless m
        exp = "UUID__HonourMode_YYYY-MM-DD-HH-MM[_note]#{EXT}"
        raise ArchiveError, "Bad archive name: #{pn.basename} (expected #{exp})"
      end
      new(save_uuid: m[1], timestamp: Time.strptime(m[2], "%Y-%m-%d-%H-%M"), custom_name: m[3], path: pn)
    end

    def self.filename_for(uuid, ts, name)
      t = ts.strftime("%Y-%m-%d-%H-%M")
      n = name ? "_#{sanitize(name)}" : ""
      "#{uuid}#{PREFIX}#{t}#{n}#{EXT}"
    end

    def self.sanitize(s) = s.to_s.downcase.tr(" ", "-").gsub(/[^a-z0-9_-]/, "").squeeze("-").gsub(/^-|-$/, "")
    def exists? = path.file?
    def size_mb = exists? ? path.size / (1024.0 * 1024.0) : 0.0
  end

  module FS
    module_function
    def scan_saves(cfg)
      cfg.save_dirs.flat_map { |d| d.children.select(&:directory?) rescue [] }
                   .map { Save.new(_1) }
                   .select(&:honour_mode?)
    end

    def scan_archives(cfg)
      return [] unless cfg.backup_dir.directory?
      cfg.backup_dir.glob("*#{Archive::PREFIX}*#{Archive::EXT}")
                    .filter_map { |p| Archive.from_path(p) rescue nil }
    end

    def archives_for_uuid(cfg, uuid)
      scan_archives(cfg).select { _1.save_uuid.casecmp?(uuid) }.sort_by(&:timestamp).reverse
    end

    def most_recent_save(saves) = saves.max_by(&:timestamp)
    def latest_archive_for(uuid, archives)
      archives.select { _1.save_uuid.casecmp?(uuid) }.max_by(&:timestamp)
    end
  end

  module Tar
    module_function
    def create(src_dir:, folder_name:, dest:)
      FileUtils.mkdir_p(dest.parent)
      out, err, st = Open3.capture3("tar", "-cJf", dest.to_s, "-C", src_dir.to_s, folder_name)
      msg = err.to_s.strip
      msg = out.to_s.strip if msg.empty?
      ok = st.success? && File.file?(dest) && File.size?(dest)
      raise ArchiveError, "tar create failed: #{msg}" unless ok
    end

    def extract(archive:, into:)
      FileUtils.mkdir_p(into)
      out, err, st = Open3.capture3("tar", "-xJf", archive.to_s, "-C", into.to_s)
      msg = err.to_s.strip
      msg = out.to_s.strip if msg.empty?
      raise ArchiveError, "tar extract failed: #{msg}" unless st.success?
    end
  end

  module App
    module_function

    # list [no args]: show all honour saves; pre-group archive counts for O(1) lookup
    def list(cfg)
      saves    = FS.scan_saves(cfg).sort_by(&:timestamp).reverse
      archives = FS.scan_archives(cfg)
      grouped  = archives.group_by(&:save_uuid)

      puts TTY.blue("=== Honour Mode Saves ===\n")
      if saves.empty?
        puts TTY.yellow("No honour mode saves found.")
        cfg.save_dirs.each { |d| puts "  #{d} #{d.exist? ? '' : '(missing)'}" }
        return
      end

      mr = FS.most_recent_save(saves)
      w = 36
      puts "  #{'UUID'.ljust(w)}  #{'Last Played'.ljust(19)}  #{'Size'.rjust(8)}  Backups"
      saves.each do |s|
        count = grouped.fetch(s.uuid, []).size
        mark  = s.uuid == mr.uuid ? TTY.cyan("→ ") : "  "
        size  = format("%6.1f MB", s.size_mb)
        puts "#{mark}#{s.uuid.ljust(w)}  #{s.timestamp_s}  #{size}  #{count}"
      end

      if (a = archives.max_by(&:timestamp))
        puts "\nDefaults:"
        puts "  create  → #{mr.short}... (#{mr.timestamp_s})"
        puts "  restore → #{a.save_uuid[0,8]}... (#{a.timestamp.strftime('%Y-%m-%d %H:%M')})"
      end
    end

    # list backups for one save (UUID). Returns true if any, false if none
    def list_backups_for_uuid(cfg, uuid)
      archives = FS.archives_for_uuid(cfg, uuid)
      saves = FS.scan_saves(cfg)
      save = saves.find { _1.uuid.casecmp?(uuid) }

      puts TTY.blue("=== Backups for #{uuid} ===\n")
      if save
        puts "Current save: #{save.timestamp_s}  #{format('%.1f MB', save.size_mb)}"
      else
        puts TTY.yellow("Save not currently present on disk.")
      end

      if archives.empty?
        puts TTY.yellow("No backups found for this save.")
        return false
      end

      print_archive_rows(archives)
      puts
      puts "Tip: restore this save's latest backup with:"
      puts "  #{$PROGRAM_NAME} restore #{uuid}"
      true
    end

    def create(cfg, uuid: nil, name: nil, force: false)
      saves = FS.scan_saves(cfg)
      raise SaveNotFoundError, "No honour mode saves found" if saves.empty?
      save = uuid ? saves.find { _1.uuid.casecmp?(uuid) } : FS.most_recent_save(saves)
      raise SaveNotFoundError, "Save not found: #{uuid}" unless save

      arch = Archive.from_save(save, backup_dir: cfg.backup_dir, name: name)
      if arch.exists?
        unless force || TTY.confirm?("Backup exists for that timestamp. Create another?")
          puts TTY.yellow("Skipped.")
          return
        end
        suffix = Time.now.strftime("manual-%H%M%S")
        arch = Archive.from_save(save, backup_dir: cfg.backup_dir, name: suffix)
      end

      cfg.ensure_backup_dir!
      original_mb = save.size_mb
      Tar.create(src_dir: save.path.dirname, folder_name: save.path.basename.to_s, dest: arch.path)
      compressed_mb = arch.size_mb
      saved_pct = original_mb.positive? ? (100.0 * (1.0 - (compressed_mb / original_mb))) : 0.0

      puts TTY.green(
        "Created: #{arch.path.basename} (#{format('%.2f MB', original_mb)} → #{format('%.2f MB', compressed_mb)}, saved #{format('%.1f', saved_pct)}%)"
      )
    end

    # Keep last N backups per save; report space freed
    def clean(cfg, keep: cfg.keep_count)
      all     = FS.scan_archives(cfg)
      before  = all.sum(&:size_mb)
      removed = 0

      all.group_by(&:save_uuid).each_value do |arr|
        arr.sort_by!(&:timestamp).reverse!
        arr.drop(keep).each { FileUtils.rm_f(_1.path); removed += 1 }
      end

      after  = FS.scan_archives(cfg).sum(&:size_mb)
      freed  = before - after

      if removed.positive?
        puts TTY.green("Removed #{removed} old backup(s). Freed #{format('%.2f MB', freed)}.")
      else
        puts TTY.cyan("No old backups to remove.")
      end
    end

    # Single prompt, no extra backup when restoring
    def restore(cfg, archive_arg, force: false)
      path = resolve_archive_arg(cfg, archive_arg)
      arch = Archive.from_path(path)
      dest = resolve_destination(cfg, arch)

      if dest.exist? && !force
        puts TTY.yellow("Destination exists: #{dest}")
        unless TTY.confirm?("Overwrite existing save? This will delete the current save.")
          puts "Cancelled."
          return
        end
      end

      FileUtils.rm_rf(dest) if dest.exist?
      Tar.extract(archive: path, into: dest.parent)
      raise ArchiveError, "Restore produced no LSV files" if dest.glob("*.lsv").empty?

      puts TTY.green("Restored → #{dest}")
      puts "Note: restart BG3; Steam Cloud may overwrite restored files."
    end

    def resolve_archive_arg(cfg, arg)
      if arg =~ /^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/i
        a = FS.latest_archive_for(arg, FS.scan_archives(cfg)) or raise ArchiveError, "No backups for #{arg}"
        return a.path
      end
      expanded = arg.sub(/^~/, ENV["HOME"])
      matches = Dir.glob(expanded)
      raise ArchiveError, "Archive not found: #{arg}" if matches.empty?
      raise ArchiveError, "Multiple matches:\n  #{matches.join("\n  ")}" if matches.size > 1
      Pathname(matches.first)
    end
    module_function :resolve_archive_arg

    def resolve_destination(cfg, arch)
      existing = FS.scan_saves(cfg).find { _1.uuid.casecmp?(arch.save_uuid) }
      return existing.path if existing
      base = cfg.save_dirs.first or raise ArchiveError, "No save directories available"
      base / "#{arch.save_uuid}__HonourMode"
    end
    module_function :resolve_destination

    def print_archive_rows(arr)
      arr.each_with_index do |a, i|
        tag = i.zero? ? TTY.cyan("→") : " "
        name = a.custom_name ? " [#{a.custom_name.tr('-', ' ')}]" : ""
        puts " #{tag} #{a.timestamp.strftime('%Y-%m-%d %H:%M')}#{name}  #{format('%.1f MB', a.size_mb)}"
      end
    end
    module_function :print_archive_rows
  end

  module CLI
    UUID_RE = /^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/i
    module_function
    def run(argv = ARGV, cfg: Config.from_env)
      cmd = (argv[0] || "list").to_s
      case cmd
      when "list"          then handle_list(cfg, argv)
      when "create"        then handle_create(cfg, argv)
      when "restore"       then argv[1] ? App.restore(cfg, argv[1], force: false) : restore_latest(cfg)
      when "clean"         then App.clean(cfg)
      when "help", "-h", "--help" then puts(help(cfg))
      else
        warn TTY.red("Unknown command: #{cmd}"); puts(help(cfg)); exit 1
      end
    rescue SaveNotFoundError, ArchiveError => e
      warn TTY.red(e.message); exit 1
    rescue Interrupt
      puts TTY.yellow("\nInterrupted"); exit 130
    end

    # list [uuid] — with UUID, show all backups for that save (exit 1 if none)
    def handle_list(cfg, argv)
      a1 = argv[1]
      if a1&.match?(UUID_RE)
        ok = App.list_backups_for_uuid(cfg, a1)
        exit 1 unless ok
      else
        App.list(cfg)
      end
    end
    module_function :handle_list

    # create:
    #   bg3.rb create                 # most recent save
    #   bg3.rb create "note here"     # most recent save + note
    #   bg3.rb create <uuid> [note]   # specific save + optional note
    def handle_create(cfg, argv)
      a1 = argv[1]
      a2 = argv[2]
      if a1.nil?
        App.create(cfg, uuid: nil, name: nil, force: false)
      elsif a1 =~ UUID_RE
        App.create(cfg, uuid: a1, name: a2, force: false)
      else
        App.create(cfg, uuid: nil, name: a1, force: false)
      end
    end
    module_function :handle_create

    def restore_latest(cfg)
      archives = FS.scan_archives(cfg)
      a = archives.max_by(&:timestamp) or raise ArchiveError, "No backups found"
      App.restore(cfg, a.path.to_s, force: false)
    end
    module_function :restore_latest

    def help(cfg)
      keep = cfg.keep_count
      <<~H
        #{TTY.blue("Baldur's Gate 3 Honour Mode Backup")}

        Usage:
          #{$PROGRAM_NAME} list [uuid]             # List saves, or backups for a specific save
          #{$PROGRAM_NAME} create [note]           # Backup most recent save, optional note
          #{$PROGRAM_NAME} create <uuid> [note]    # Backup specific save, optional note
          #{$PROGRAM_NAME} restore [path|uuid]     # Restore most recent or specific backup
          #{$PROGRAM_NAME} clean                   # Keep last #{keep} per save

        Notes:
          • Default "create" backs up the most recent save (you can add a note).
          • Default "restore" restores the most recent backup into its corresponding save.
          • Pass a UUID to "list" to see all backups for that save.
          • Pass a UUID to "restore" to restore the latest backup for that save.

        ENV:
          STEAM_BASE, BG3_BACKUP_DIR, KEEP_BACKUPS
          NO_COLOR=1        # disable colours
          BG3_YES=1         # auto-confirm prompts
      H
    end
  end
end

BG3::CLI.run if __FILE__ == $PROGRAM_NAME
