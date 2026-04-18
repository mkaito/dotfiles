# frozen_string_literal: true

require "fileutils"
require_relative "../collection"
require_relative "../errors"

module ModManager
  module Interactors
    class ImportCollection
      def initialize(provider:, download:, archive:, catalog:, terminal:)
        @provider = provider
        @download = download
        @archive  = archive
        @catalog  = catalog
        @terminal = terminal
      end

      def call(collection_id, revision: nil)
        rev      = @provider.fetch_revision(slug: collection_id, revision:)
        col_name = "nexus-#{collection_id}-#{rev.collection_name}-#{rev.revision_number}"

        slugs = rev.mods.map do |entry|
          existing = find_in_archive(entry.mod_id, entry.file_id)
          if existing
            @terminal.info("skip #{existing.slug} (already archived)")
            next existing.slug
          end

          mod = install_mod(entry.mod_id, entry.file_id)
          @terminal.success("archived #{mod.slug}")
          mod.slug
        end

        collection = Collection.new(name: col_name, mods: slugs.compact, path: nil)
        @catalog.write_collection(collection)
        @terminal.success("imported collection #{col_name} (#{slugs.compact.size} mods)")
        collection
      end

      private

      def install_mod(mod_id, file_id)
        unpacked = @download.fetch(mod_id:, file_id:)
        @archive.install(unpacked_mod: unpacked)
      ensure
        FileUtils.rm_rf(unpacked.tmp_dir) if unpacked
      end

      def find_in_archive(mod_id, file_id)
        @archive.all.find do |m|
          m.source["provider"] == "nexus" &&
            m.source["mod_id"] == mod_id &&
            m.source["file_id"] == file_id
        end
      end
    end
  end
end
