# frozen_string_literal: true

require "mod_manager/modset_editor"
require "mod_manager/modset"
require "mod_manager/errors"

module ModManager
  module Interactors
    class ModsetCrud
      def initialize(catalog:, terminal:, game:, modsets_dir:)
        @catalog     = catalog
        @terminal    = terminal
        @game        = game
        @modsets_dir = modsets_dir
      end

      def new_modset(name)
        raise Error, "modset already exists: #{name}" if @catalog.modset_exist?(name)
        ms_path = File.join(@modsets_dir, "#{name}.toml")
        @catalog.write_modset(Modset.new(game: @game, collections: [], mods: [], checks: [], path: ms_path))
        @terminal.success("created #{name}")
      end

      def list
        names = @catalog.list_modsets
        if names.empty?
          @terminal.dim("no modsets")
          return
        end
        names.each do |ms_name|
          ms = @catalog.read_modset(ms_name)
          @terminal.info("#{@terminal.bold(ms_name)}: #{ms.collections.size} collection(s)")
        rescue ValidationError => e
          @terminal.warn("#{@terminal.bold(ms_name)}: (invalid: #{e.errors.first})")
        end
      end

      def show(name)
        ms = @catalog.read_modset(name)
        if ms.collections.empty? && ms.mods.empty?
          @terminal.info("(empty)")
        else
          ms.collections.each { @terminal.info(_1) }
          ms.mods.each { @terminal.info("  #{_1} (direct mod)") } if ms.mods.any?
        end
      end

      def add(name, collection_names)
        ModsetEditor.add(name, collection_names, @catalog)
        @terminal.success("updated #{name}")
      end

      def remove(name, collection_name)
        ModsetEditor.remove(name, collection_name, @catalog)
        @terminal.success("updated #{name}")
      end

      def delete(name, yes: false)
        raise Error, "modset not found: #{name}" unless @catalog.modset_exist?(name)
        unless yes || @terminal.confirm("delete #{name}?")
          @terminal.info("aborted")
          return
        end
        @catalog.delete_modset(name)
        @terminal.success("deleted #{name}")
      end
    end
  end
end
