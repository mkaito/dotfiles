# frozen_string_literal: true

require "mod_manager/collection_editor"
require "mod_manager/collection"
require "mod_manager/errors"

module ModManager
  module Interactors
    class CollectionCrud
      def initialize(catalog:, terminal:, archive: nil)
        @catalog  = catalog
        @terminal = terminal
        @archive  = archive
      end

      def new_collection(name)
        raise Error, "collection already exists: #{name}" if @catalog.collection_exist?(name)
        @catalog.write_collection(Collection.new(name:, mods: [], path: nil))
        @terminal.success("created #{name}")
      end

      def list
        names = @catalog.list_collections
        if names.empty?
          @terminal.dim("no collections")
          return
        end
        names.each do |col_name|
          col = @catalog.read_collection(col_name)
          @terminal.info("#{@terminal.bold(col_name)}: #{col.mods.size} mod(s)")
        rescue ValidationError => e
          @terminal.warn("#{@terminal.bold(col_name)}: (invalid: #{e.errors.first})")
        end
      end

      def show(name)
        col = @catalog.read_collection(name)
        if col.mods.empty?
          @terminal.info("(empty)")
        else
          col.mods.each { @terminal.info(_1) }
        end
      end

      def add(collection_name, slugs)
        CollectionEditor.add(collection_name, slugs, @archive, @catalog)
        @terminal.success("updated #{collection_name}")
      end

      def remove(collection_name, slug)
        CollectionEditor.remove(collection_name, slug, @catalog)
        @terminal.success("updated #{collection_name}")
      end

      def delete(name, yes: false)
        raise Error, "collection not found: #{name}" unless @catalog.collection_exist?(name)
        unless yes || @terminal.confirm("delete #{name}?")
          @terminal.info("aborted")
          return
        end
        @catalog.delete_collection(name)
        @terminal.success("deleted #{name}")
      end
    end
  end
end
