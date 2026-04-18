# frozen_string_literal: true

require_relative "../errors"

module ModManager
  module Interactors
    class ListMods
      def initialize(archive:, catalog:, terminal:)
        @archive  = archive
        @catalog  = catalog
        @terminal = terminal
      end

      def call(orphans_only: false)
        if @archive.all.empty?
          @terminal.dim("archive empty")
          return
        end

        membership = Hash.new { |h, k| h[k] = [] }
        @catalog.list_collections.each do |col_name|
          @catalog.read_collection(col_name).mods.each { membership[_1] << col_name }
        rescue ValidationError
          # skip invalid collections silently
        end

        mods = @archive.all.sort_by(&:slug)
        mods = mods.reject { membership[_1.slug].any? } if orphans_only

        if mods.empty?
          @terminal.dim("no uncollected mods")
          return
        end

        width = mods.map { _1.slug.length }.max
        mods.each do |mod|
          cols = membership[mod.slug]
          if cols.any?
            @terminal.info("#{mod.slug.ljust(width)}  #{@terminal.muted("[#{cols.join(", ")}]")}")
          else
            @terminal.info(mod.slug)
          end
        end
      end
    end
  end
end
