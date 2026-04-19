# frozen_string_literal: true

require "fileutils"
require "core/text"
require "mod_manager/collection"
require "mod_manager/modset"
require "mod_manager/errors"
require "nexus/fomod"

module ModManager
  module Interactors
    class ImportCollection
      def initialize(provider:, download:, archive:, catalog:, terminal:, game:)
        @provider = provider
        @download = download
        @archive = archive
        @catalog = catalog
        @terminal = terminal
        @game = game
      end

      def call(collection_id, revision: nil)
        rev = @provider.fetch_revision(slug: collection_id, revision:)
        manifest = @provider.fetch_manifest(download_link: rev.download_link,
          slug: collection_id, revision: rev.revision_number)
        manifest_by_file_id = manifest.each_with_object({}) { |m, h| h[m.file_id] = m }
        archive_index = @archive.all.each_with_object({}) do |m, h|
          h[[m.source["provider"], m.source["mod_id"], m.source["file_id"]]] = m
        end

        col_base = "nexus-#{rev.collection_id}-#{rev.collection_name}-#{rev.revision_number}"
        fomod_split = nil   # Hash{choice_name => slug} when an unresolved FOMOD is found
        shared_slugs = []

        verify_all!(rev.mods, rev.collection_id, archive_index)

        rev.mods.each do |entry|
          existing = archive_index[["nexus", entry.mod_id, entry.file_id]]
          if existing
            @terminal.info("skip #{existing.slug} (already archived)")
            shared_slugs << existing.slug
            next
          end

          result = install_mod(entry, manifest_by_file_id[entry.file_id])

          if result.is_a?(Hash)
            raise Error, "multiple FOMOD mods in one collection not yet supported" if fomod_split
            fomod_split = result
          else
            @terminal.success("archived #{result}")
            shared_slugs << result
          end
        end

        if fomod_split
          fomod_split.each do |choice_name, fomod_slug|
            col_name = "nexus-#{rev.collection_id}-#{Core::Text.slugify(choice_name)}-#{rev.revision_number}"
            write_collection_and_modset(col_name, shared_slugs + [fomod_slug])
            @terminal.success("created #{col_name}")
          end
        else
          write_collection_and_modset(col_base, shared_slugs)
          @terminal.success("imported collection #{col_base} (#{shared_slugs.size} mods)")
        end
      end

      private

      def install_mod(entry, manifest_mod)
        unpacked = @download.fetch(mod_id: entry.mod_id, file_id: entry.file_id)
        choices = ::Nexus::Fomod.detect(unpacked.tmp_dir)

        if choices.nil?
          mod = @archive.install(unpacked_mod: unpacked)
          return mod.slug
        end

        if manifest_mod&.choices
          # Pre-selected FOMOD — archive the whole extracted dir as-is.
          # Vortex applies the pre-selected options at deploy time; we just store the files.
          mod = @archive.install(unpacked_mod: unpacked)
          return mod.slug
        end

        # Unresolved FOMOD — archive each choice variant as a separate slug.
        result = {}
        choices.each do |choice|
          choice_dir = File.join(unpacked.tmp_dir, "__choice__#{Core::Text.slugify(choice.name)}")
          FileUtils.mkdir_p(choice_dir)
          choice.folders.each do |folder|
            src = File.join(unpacked.tmp_dir, folder["source"])
            dest = folder["destination"].to_s.empty? ? choice_dir : File.join(choice_dir, folder["destination"])
            FileUtils.mkdir_p(dest)
            Dir.glob("#{src}/*").each { FileUtils.cp_r(it, dest) }
          end
          FileUtils.chmod_R(0o755, choice_dir)
          choice_mod = UnpackedMod.new(
            tmp_dir: choice_dir,
            slug: "#{unpacked.slug}--#{Core::Text.slugify(choice.name)}",
            version: unpacked.version,
            game: unpacked.game,
            name: "#{unpacked.name} (#{choice.name})",
            source: unpacked.source.merge("fomod_choice" => choice.name)
          )
          archived = @archive.install(unpacked_mod: choice_mod)
          @terminal.success("archived #{archived.slug} (FOMOD: #{choice.name})")
          result[choice.name] = archived.slug
        end
        result
      ensure
        FileUtils.rm_rf(unpacked.tmp_dir) if unpacked
      end

      def write_collection_and_modset(name, slugs)
        collection = Collection.new(name:, mods: slugs, path: nil)
        @catalog.write_collection(collection)
        modset = Modset.new(game: @game, collections: [name], mods: [], checks: [], path: "#{name}.toml")
        @catalog.write_modset(modset)
      end

      def verify_all!(mods, collection_id, archive_index)
        missing = mods.reject do |m|
          archive_index[["nexus", m.mod_id, m.file_id]] || @download.file_exist?(mod_id: m.mod_id, file_id: m.file_id)
        end
        return if missing.empty?

        missing.each do |m|
          url = "https://www.nexusmods.com/#{@game}/mods/#{m.mod_id}?tab=files&file_id=#{m.file_id}"
          @terminal.warn("missing: mod #{m.mod_id} file #{m.file_id} — #{url}")
        end
        raise Core::Error,
          "#{missing.size} mod file(s) no longer available on Nexus; collection #{collection_id} cannot be imported"
      end
    end
  end
end
