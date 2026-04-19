# frozen_string_literal: true

require "fileutils"
require "mod_manager/errors"

module ModManager
  module Interactors
    class RepairArchive
      def initialize(archive:, download:, terminal:)
        @archive = archive
        @download = download
        @terminal = terminal
      end

      def call(mods: nil)
        candidates = @archive.all.select { it.source["provider"] == "nexus" }
        candidates = candidates.select { mods.include?([it.source["mod_id"], it.source["file_id"]]) } if mods

        if candidates.empty?
          @terminal.info("no matching mods in archive")
          return
        end

        @terminal.info("rebuilding #{candidates.size} mod(s)")
        failed = []

        candidates.each do |mod|
          unpacked = nil
          begin
            unpacked = @download.fetch(mod_id: mod.source["mod_id"], file_id: mod.source["file_id"],
              slug: mod.slug)
            @archive.delete(mod)
            @archive.install(unpacked_mod: unpacked)
            @terminal.info("#{@terminal.green("rebuilt")} #{mod.slug}")
          rescue Core::Error => e
            @terminal.error("failed #{mod.slug}: #{e.message}")
            failed << mod.slug
          ensure
            FileUtils.rm_rf(unpacked.tmp_dir) if unpacked
          end
        end

        @terminal.warn("#{failed.size} mod(s) failed — re-download manually") if failed.any?
      end
    end
  end
end
