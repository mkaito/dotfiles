# frozen_string_literal: true

require_relative "../../collection_revision"
require_relative "../../errors"

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
            col_name = rev_data["collection"]["name"]
            rev_num  = rev_data["revisionNumber"]
            mods_raw = rev_data["modFiles"]
          else
            data     = @client.collection_revision(@game_domain, slug)
            col_data = data["collection"] or
                       raise Error, "no collection in response for #{slug}"
            col_name = col_data["name"]
            rev_data = col_data["latestPublishedRevision"] or
                       raise Error, "no latestPublishedRevision for collection #{slug}"
            rev_num  = rev_data["revisionNumber"]
            mods_raw = rev_data["modFiles"]
          end

          mods = (mods_raw || []).map do |m|
            CollectionRevisionMod.new(
              mod_id:       m["file"]["modId"],
              file_id:      m["fileId"],
              file_name:    m["file"]["name"],
              file_version: m["file"]["version"],
            )
          end

          CollectionRevision.new(
            collection_id:   slug,
            collection_name: slugify(col_name),
            revision_number: rev_num,
            mods:,
          )
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

        def slugify(name)
          name.downcase.gsub(/[^a-z0-9]+/, "-").gsub(/\A-+|-+\z/, "")
        end
      end
    end
  end
end
