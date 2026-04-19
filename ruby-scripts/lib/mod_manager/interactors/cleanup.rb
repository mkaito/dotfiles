# frozen_string_literal: true

require "mod_manager/log"
require "mod_manager/errors"

module ModManager
  module Interactors
    class Cleanup
      def initialize(archive:, catalog:, terminal:)
        @archive = archive
        @catalog = catalog
        @terminal = terminal
      end

      def call(dry_run: false, yes: false)
        referenced = Set.new

        @catalog.list_collections.each do |col_name|
          @catalog.read_collection(col_name).mods.each { referenced << it }
        rescue ValidationError => e
          e.errors.each { @terminal.warn("warning: #{it}") }
        end
        @catalog.list_modsets.each do |ms_name|
          @catalog.read_modset(ms_name).mods.each { referenced << it }
        rescue ValidationError => e
          e.errors.each { @terminal.warn("warning: #{it}") }
        end

        orphans = @archive.all.reject { referenced.include?(it.slug) }

        if orphans.empty?
          @terminal.info("no orphaned mods")
          return
        end

        @terminal.info("orphaned (#{orphans.size}):")
        orphans.each { @terminal.info("  #{it}") }
        return if dry_run

        unless yes || @terminal.confirm("delete #{orphans.size} mod(s)?")
          @terminal.info("aborted")
          return
        end

        orphans.each do |mod|
          @archive.delete(mod)
          Log.info("deleted #{mod}")
        end
        @terminal.success("deleted #{orphans.size} mod(s)")
      end
    end
  end
end
