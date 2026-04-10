#!/usr/bin/env ruby
# frozen_string_literal: true

# NMS save format (observed 2025, keys from goatfungus/NMSSaveEditor db/jsonmap.txt NMS 5.21)
#
# PATH  st_<STEAMID>/save.hg, save2.hg…save14.hg  (slot 1 = no suffix)
#       mf_save*.hg  ~432B companion manifest, different binary format, not LZ4
#
# BINARY  [0–3] magic e5 a1 ed fe  [4–15] metadata  [16–…] LZ4 raw blocks
#         → JSON with obfuscated 3-char keys:
#   <h0  CommonStateData      Pk4  SaveName (may be empty)
#   7ND  DifficultyPresetType "Permadeath"/"Normal"/… (also "Invalid" in expedition
#                             sub-objects; "7ND":"Permadeath" in first ~35KB = reliable)
#   K7E  UID (Steam ID)       F2P  Version (counter, same in both paired slots)
#   8sY  SeasonalUAOverride   — NOT a character identity
#
# SLOTS  NMS writes each character to TWO consecutive slots (SlotNAuto + SlotNManual)
#        pairs: 1&2, 3&4, 5&6, 7&8, 9&10, 11&12, 13&14
#        lower (odd) slot = canonical identity; no stable char UUID exists in save data

require "pathname"
require "time"
require "fileutils"
require "open3"

module NMS
  Error             = Class.new(StandardError)
  SaveNotFoundError = Class.new(Error)
  ArchiveError      = Class.new(Error)

  Config = Struct.new(:save_base, :backup_dir, :keep_count, keyword_init: true) do
    def self.from_env
      new(
        save_base:  Pathname(ENV.fetch("NMS_SAVE_BASE",
          "#{ENV.fetch('HOME')}/.local/share/Steam/steamapps/compatdata/275850/pfx/" \
          "drive_c/users/steamuser/AppData/Roaming/HelloGames/NMS")).expand_path,
        backup_dir: Pathname(ENV.fetch("NMS_BACKUP_DIR",
          "#{ENV.fetch('HOME')}/Documents/NMS_Backups")).expand_path,
        keep_count: Integer(ENV.fetch("KEEP_BACKUPS", "5"))
      )
    end

    def save_dir
      @save_dir ||= begin
        dirs = save_base.glob("st_*").select(&:directory?)
        raise Error, "no save dir: #{save_base}" if dirs.empty?
        dirs.first
      end
    end

    def save_files
      @save_files ||= (1..14).filter_map do |n|
        sfx  = n == 1 ? "" : n.to_s
        save = save_dir / "save#{sfx}.hg"
        mf   = save_dir / "mf_save#{sfx}.hg"
        { slot: n, save: save, mf: mf } if save.file?
      end
    end

    def ensure_backup_dir! = backup_dir.mkpath
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
      return true if ENV["NMS_YES"] == "1"
      print yellow("#{q} (y/N): ")
      ans = $stdin.gets.to_s.strip.downcase
      ans == "y" || ans == "yes"
    end
  end

  # Minimal LZ4 raw-block decoder. NMS .hg files: 16-byte header, then LZ4 raw blocks.
  module LZ4
    module_function
    def decode_raw(data, max_out: 35_000)
      out = +"".b
      i   = 0
      while i < data.bytesize
        tok = data.getbyte(i); i += 1
        lit = tok >> 4
        if lit == 15
          loop { b = data.getbyte(i); i += 1; lit += b; break if b < 255 }
        end
        out << data.byteslice(i, lit); i += lit
        return out if out.bytesize >= max_out || i >= data.bytesize
        off = data.getbyte(i) | (data.getbyte(i + 1) << 8); i += 2
        mat = (tok & 0xF) + 4
        if (tok & 0xF) == 15
          loop { b = data.getbyte(i); i += 1; mat += b; break if b < 255 }
        end
        src = out.bytesize - off
        mat.times { out << out.getbyte(src).chr.b; src += 1 }
        return out if out.bytesize >= max_out
      end
      out
    rescue
      out
    end
  end

  # Represents a canonical slot pair (Auto + Manual). The lower-numbered
  # (odd) slot is primary; the partner is the even slot, if present.
  class Slot
    attr_reader :canonical_slot, :save_path, :mf_path, :partner_save, :partner_mf

    def initialize(canonical_slot:, save_path:, mf_path:, partner_save: nil, partner_mf: nil)
      @canonical_slot = canonical_slot
      @save_path      = save_path
      @mf_path        = mf_path
      @partner_save   = partner_save
      @partner_mf     = partner_mf
    end

    # All save/mf files present on disk for this pair (used by Tar)
    def all_saves = [@save_path, @partner_save].compact
    def all_mfs   = [@mf_path, @partner_mf].compact.select(&:file?)

    def mtime      = @mtime   ||= all_saves.map(&:mtime).max
    def size_mb    = @size_mb ||= all_saves.sum(&:size) / (1024.0 * 1024.0)
    def timestamp_s = mtime.strftime("%Y-%m-%d %H:%M:%S")

    def name
      @name ||= begin
        raw     = File.binread(save_path, 2_000)
        decoded = LZ4.decode_raw(raw.byteslice(16..), max_out: 500)
        decoded[/"Pk4":"([^"]*)"/, 1] || ""
      rescue
        ""
      end
    end

    def permadeath? = FS._permadeath?(save_path)
  end

  class Archive
    EXT = ".tar.xz"
    RGX = /^NMS_slot(\d+)_(\d{4}-\d{2}-\d{2}-\d{2}-\d{2})(?:_(.+))?\.tar\.xz$/

    attr_reader :slot, :timestamp, :custom_name, :path

    def initialize(slot:, timestamp:, custom_name: nil, path:)
      @slot, @timestamp, @custom_name, @path = slot, timestamp, custom_name, Pathname(path)
    end

    def self.from_slot(slot_obj, backup_dir:, name: nil)
      name = sanitize(name) if name
      new(
        slot:        slot_obj.canonical_slot,
        timestamp:   slot_obj.mtime,
        custom_name: name,
        path:        backup_dir / filename_for(slot_obj.canonical_slot, slot_obj.mtime, name)
      )
    end

    def self.from_path(path)
      pn = Pathname(path)
      m  = pn.basename.to_s.match(RGX)
      raise ArchiveError, "Bad archive name: #{pn.basename}" unless m
      new(slot: m[1].to_i, timestamp: Time.strptime(m[2], "%Y-%m-%d-%H-%M"),
          custom_name: m[3], path: pn)
    end

    def self.filename_for(slot, ts, name)
      n = name ? "_#{name}" : ""
      "NMS_slot#{slot}_#{ts.strftime('%Y-%m-%d-%H-%M')}#{n}#{EXT}"
    end

    def self.sanitize(s) = s.to_s.downcase.tr(" ", "-").gsub(/[^a-z0-9_-]/, "").squeeze("-").gsub(/^-|-$/, "")
    def exists?  = path.file?
    def size_mb  = exists? ? path.size / (1024.0 * 1024.0) : 0.0
  end

  module FS
    module_function

    # One Slot per character pair, carrying both Auto and Manual files.
    # Permadeath is checked on each file individually before pairing.
    def scan_slots(cfg)
      permadeath_files = cfg.save_files.select { |f| _permadeath?(f[:save]) }

      permadeath_files.group_by { |f| f[:slot].odd? ? f[:slot] : f[:slot] - 1 }.map do |canonical, files|
        primary = files.find { |f| f[:slot] == canonical } || files.min_by { |f| f[:slot] }
        partner = files.find { |f| f[:slot] != primary[:slot] }
        Slot.new(
          canonical_slot: canonical,
          save_path:      primary[:save],
          mf_path:        primary[:mf],
          partner_save:   partner&.dig(:save),
          partner_mf:     partner&.dig(:mf)
        )
      end
    end

    def _permadeath?(path)
      raw     = File.binread(path, 60_000)
      decoded = LZ4.decode_raw(raw.byteslice(16..), max_out: 35_000)
      decoded.include?('"7ND":"Permadeath"')
    rescue
      false
    end
    module_function :_permadeath?

    def scan_archives(cfg)
      return [] unless cfg.backup_dir.directory?
      cfg.backup_dir.glob("NMS_slot*#{Archive::EXT}")
                    .filter_map { Archive.from_path(_1) rescue nil }
    end

    def archives_for_slot(cfg, slot)
      # Accept either slot of a pair
      canonical = slot.odd? ? slot : slot - 1
      scan_archives(cfg).select { _1.slot == canonical }.sort_by(&:timestamp).reverse
    end

    def most_recent_slot(slots) = slots.max_by(&:mtime)
    def latest_archive_for(slot, archives)
      canonical = slot.odd? ? slot : slot - 1
      archives.select { _1.slot == canonical }.max_by(&:timestamp)
    end
  end

  module Tar
    module_function

    def create(slot:, dest:)
      FileUtils.mkdir_p(dest.parent)
      save_dir = slot.save_path.parent
      files    = slot.all_saves.map { _1.basename.to_s } +
                 slot.all_mfs.map  { _1.basename.to_s }
      out, err, st = Open3.capture3("tar", "-cJf", dest.to_s, "-C", save_dir.to_s, *files)
      msg = err.to_s.strip.then { _1.empty? ? out.to_s.strip : _1 }
      raise ArchiveError, "tar create failed: #{msg}" unless st.success? && dest.file? && dest.size?
    end

    def extract(archive:, into:)
      FileUtils.mkdir_p(into)
      out, err, st = Open3.capture3("tar", "-xJf", archive.to_s, "-C", into.to_s)
      msg = err.to_s.strip.then { _1.empty? ? out.to_s.strip : _1 }
      raise ArchiveError, "tar extract failed: #{msg}" unless st.success?
    end
  end

  module App
    module_function

    def fmt_mb(mb) = mb < 1.0 ? format("%.0f KB", mb * 1024) : format("%.1f MB", mb)

    def list(cfg)
      slots    = FS.scan_slots(cfg).sort_by(&:mtime).reverse
      archives = FS.scan_archives(cfg)
      grouped  = archives.group_by(&:slot)

      puts TTY.blue("=== Permadeath Saves ===\n")
      if slots.empty?
        puts TTY.yellow("no permadeath saves")
        puts "  dir: #{cfg.save_dir rescue cfg.save_base}"
        return
      end

      mr = FS.most_recent_slot(slots)
      puts "  #{'slot'.ljust(5)}  #{'name'.ljust(20)}  #{'saved'.ljust(19)}  #{'size'.rjust(8)}  bkp"
      slots.each do |s|
        count      = grouped.fetch(s.canonical_slot, []).size
        mark       = s.canonical_slot == mr.canonical_slot ? TTY.cyan("→") : " "
        label      = s.name.empty? ? TTY.yellow("unnamed") : s.name
        size       = fmt_mb(s.size_mb).rjust(8)
        pair_label = s.partner_save ? "#{s.canonical_slot}+#{s.canonical_slot + 1}" : s.canonical_slot.to_s
        puts " #{mark} #{pair_label.ljust(5)}  #{label.ljust(20)}  #{s.timestamp_s}  #{size}  #{count}"
      end

      if (a = archives.max_by(&:timestamp))
        puts "\ncreate → #{mr.canonical_slot}  restore → #{a.slot}"
      end
    end

    def list_backups_for_slot(cfg, slot)
      archives = FS.archives_for_slot(cfg, slot)
      s        = FS.scan_slots(cfg).find { _1.canonical_slot == (slot.odd? ? slot : slot - 1) }

      canonical = slot.odd? ? slot : slot - 1
      puts TTY.blue("=== Slot #{canonical} ===\n")
      if s
        label = s.name.empty? ? "unnamed" : s.name
        puts "  live: #{label}  #{s.timestamp_s}  #{fmt_mb(s.size_mb)}"
      else
        puts TTY.yellow("  slot #{canonical}: no live save")
      end

      if archives.empty?
        puts TTY.yellow("  no backups")
        return false
      end

      print_archive_rows(archives)
      puts
      puts "  restore: #{$PROGRAM_NAME} restore #{canonical}"
      true
    end

    def create(cfg, slot:, name:)
      slots = FS.scan_slots(cfg)
      raise SaveNotFoundError, "no permadeath saves" if slots.empty?

      s = if slot
        canonical = slot.odd? ? slot : slot - 1
        slots.find { _1.canonical_slot == canonical } or
          raise SaveNotFoundError, "no permadeath save: slot #{slot}"
      else
        FS.most_recent_slot(slots)
      end

      arch = Archive.from_slot(s, backup_dir: cfg.backup_dir, name: name)
      if arch.exists?
        unless TTY.confirm?("backup exists. create another?")
          puts TTY.yellow("Skipped.")
          return
        end
        suffix = Time.now.strftime("manual-%H%M%S")
        arch = Archive.from_slot(s, backup_dir: cfg.backup_dir, name: suffix)
      end

      cfg.ensure_backup_dir!
      original_mb = s.size_mb
      Tar.create(slot: s, dest: arch.path)
      compressed_mb = arch.size_mb
      saved_pct = original_mb.positive? ? (100.0 * (1.0 - (compressed_mb / original_mb))) : 0.0

      puts TTY.green("+ #{arch.path.basename}  #{fmt_mb(original_mb)}→#{fmt_mb(compressed_mb)}  -#{format('%.1f', saved_pct)}%")
    end

    def restore(cfg, archive_arg)
      path = resolve_archive_arg(cfg, archive_arg)
      arch = Archive.from_path(path)

      live = FS.scan_slots(cfg).find { _1.canonical_slot == arch.slot }
      puts TTY.blue("restore slot #{arch.slot}:")
      if live
        label = live.name.empty? ? "unnamed" : live.name
        puts "  live: #{label}  #{live.timestamp_s}  #{fmt_mb(live.size_mb)}"
      else
        puts "  live: #{TTY.yellow('(none)')}"
      end
      note = arch.custom_name ? " [#{arch.custom_name.tr('-', ' ')}]" : ""
      puts "  bkp:  #{arch.timestamp.strftime('%Y-%m-%d %H:%M')}#{note}  #{fmt_mb(arch.size_mb)}"
      puts

      unless TTY.confirm?("overwrite?")
        puts "cancelled"
        return
      end

      Tar.extract(archive: path, into: cfg.save_dir)

      sfx      = arch.slot == 1 ? "" : arch.slot.to_s
      restored = cfg.save_dir / "save#{sfx}.hg"
      raise ArchiveError, "no .hg after restore: slot #{arch.slot}" unless restored.file?

      puts TTY.green("ok → #{restored}")
      puts "restart NMS. steam cloud may overwrite."
    end

    def clean(cfg, keep: cfg.keep_count)
      all     = FS.scan_archives(cfg)
      before  = all.sum(&:size_mb)
      removed = 0

      all.group_by(&:slot).each_value do |arr|
        arr.sort_by!(&:timestamp).reverse!
        arr.drop(keep).each { FileUtils.rm_f(_1.path); removed += 1 }
      end

      after = FS.scan_archives(cfg).sum(&:size_mb)
      freed = before - after

      if removed.positive?
        puts TTY.green("-#{removed} bkp  freed #{format('%.2f MB', freed)}")
      else
        puts TTY.cyan("nothing to remove")
      end
    end

    def resolve_archive_arg(cfg, arg)
      if arg.match?(/^\d+$/)
        a = FS.latest_archive_for(arg.to_i, FS.scan_archives(cfg))
        raise ArchiveError, "no backups: slot #{arg}" unless a
        return a.path
      end
      expanded = arg.sub(/^~/, ENV["HOME"])
      matches  = Dir.glob(expanded)
      raise ArchiveError, "archive not found: #{arg}" if matches.empty?
      raise ArchiveError, "multiple matches:\n  #{matches.join("\n  ")}" if matches.size > 1
      Pathname(matches.first)
    end
    module_function :resolve_archive_arg

    def print_archive_rows(arr)
      arr.each_with_index do |a, i|
        tag  = i.zero? ? TTY.cyan("→") : " "
        name = a.custom_name ? " [#{a.custom_name.tr('-', ' ')}]" : ""
        puts " #{tag} #{a.timestamp.strftime('%Y-%m-%d %H:%M')}#{name}  #{fmt_mb(a.size_mb)}"
      end
    end
    module_function :print_archive_rows
  end

  module CLI
    module_function

    def run(argv = ARGV, cfg: Config.from_env)
      case (argv[0] || "list")
      when "list"                 then handle_list(cfg, argv)
      when "create"               then handle_create(cfg, argv)
      when "restore"              then argv[1] ? App.restore(cfg, argv[1]) : restore_latest(cfg)
      when "clean"                then App.clean(cfg)
      when "help", "-h", "--help" then puts help(cfg)
      else
        warn TTY.red("unknown command: #{argv[0]}"); puts help(cfg); exit 1
      end
    rescue SaveNotFoundError, ArchiveError => e
      warn TTY.red(e.message); exit 1
    rescue Interrupt
      puts TTY.yellow("\nInterrupted"); exit 130
    end

    def handle_list(cfg, argv)
      a1 = argv[1]
      return App.list(cfg) if a1.nil?
      unless a1.match?(/^\d+$/)
        warn TTY.red("list: expected slot#, got #{a1.inspect}"); exit 1
      end
      ok = App.list_backups_for_slot(cfg, a1.to_i)
      exit 1 unless ok
    end
    module_function :handle_list

    def handle_create(cfg, argv)
      a1, a2 = argv[1], argv[2]
      if a1.nil?
        App.create(cfg, slot: nil, name: nil)
      elsif a1.match?(/^\d+$/)
        App.create(cfg, slot: a1.to_i, name: a2)
      else
        App.create(cfg, slot: nil, name: a1)
      end
    end
    module_function :handle_create

    def restore_latest(cfg)
      a = FS.scan_archives(cfg).max_by(&:timestamp)
      raise ArchiveError, "no backups" unless a
      App.restore(cfg, a.path.to_s)
    end
    module_function :restore_latest

    def help(cfg)
      keep = cfg.keep_count
      <<~H
        #{TTY.blue("nms_save_manager")}

          list [slot]          permadeath saves, or backups for slot
          create [note]        backup most recent save
          create <slot> [note] backup slot pair
          restore [slot|path]  restore latest or specific backup
          clean                keep last #{keep} per slot

          slots 13+14 = same pair; either accepted

        ENV: NMS_SAVE_BASE  NMS_BACKUP_DIR  KEEP_BACKUPS  NO_COLOR=1  NMS_YES=1
      H
    end
    module_function :help
  end
end

NMS::CLI.run if __FILE__ == $PROGRAM_NAME
