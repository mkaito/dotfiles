# frozen_string_literal: true

require "tmpdir"
require "json"
require "fileutils"
require "core/text"
require "mod_manager/collection_revision"
require "mod_manager/core/models"
require "mod_manager/errors"
require "mod_manager/adapters/collection_provider/translator"
require "core/http"
require "core/xdg"

module ModManager
  module Adapters
    module CollectionProvider
      class Nexus
        def initialize(game_domain, client)
          @game_domain = game_domain
          @client      = client
        end

        def fetch_revision(slug:, revision: nil)
          if revision
            data     = @client.collection_revision(@game_domain, slug, revision:)
            rev_data = data["collectionRevision"] or
                       raise Error, "no collectionRevision in response for #{slug}@#{revision}"
            col_name      = rev_data["collection"]["name"]
            rev_num       = rev_data["revisionNumber"]
            download_link = rev_data["downloadLink"]
            mods_raw      = rev_data["modFiles"]
          else
            data     = @client.collection_revision(@game_domain, slug)
            col_data = data["collection"] or
                       raise Error, "no collection in response for #{slug}"
            col_name = col_data["name"]
            rev_data = col_data["latestPublishedRevision"] or
                       raise Error, "no latestPublishedRevision for collection #{slug}"
            rev_num       = rev_data["revisionNumber"]
            download_link = rev_data["downloadLink"]
            mods_raw      = rev_data["modFiles"]
          end

          mods = (mods_raw || []).map { Translator.revision_mod(_1) }

          CollectionRevision.new(
            collection_id:   slug,
            collection_name: Core::Text.slugify(col_name),
            revision_number: rev_num,
            download_link:,
            mods:,
          )
        end

        def fetch_manifest(download_link:, slug:, revision:)
          cached = cached_manifest(slug, revision, download_link)
          Dir.mktmpdir("collection-manifest-") do |tmp|
            system("7za", "e", "-y", "-bso0", "-bsp0", "-o#{tmp}", cached, "collection.json")
            data = JSON.parse(File.read(File.join(tmp, "collection.json")))
            (data["mods"] || []).map { Translator.manifest_mod(_1) }
          end
        end

        def list_revisions(slug:)
          data = @client.collection_revisions(@game_domain, slug)
          revs = data.dig("collection", "revisions") or
                 raise Error, "no revisions in response for #{slug}"
          revs.map do |r|
            CollectionRevisionSummary.new(
              revision_number: r["revisionNumber"],
              created_at:      r["createdAt"],
              mod_count:       r["modCount"],
              status:          r["revisionStatus"],
            )
          end
        end

        private

        def cached_manifest(slug, revision, download_link)
          path = File.join(Core::XDG.cache_home, "mods", "nexus", "collections",
                           @game_domain.to_s, "#{slug}-#{revision}.7z")
          Core::Http.cached_download(path, label: "collection manifest #{slug} r#{revision}") do
            @client.collection_download_url(download_link)
          end
        end
      end
    end
  end
end
