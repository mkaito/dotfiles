# frozen_string_literal: true

module ModManager
  module Ports
    # Fetch collection metadata from an external provider. Returns core models;
    # all provider specifics (HTTP, auth, GraphQL) stay inside the adapter.
    module CollectionProvider
      # → CollectionRevision
      def fetch_revision(slug:, revision: nil) = raise(NotImplementedError)

      # → Array<CollectionRevisionSummary>
      def list_revisions(slug:)               = raise(NotImplementedError)

      # → Array<CollectionManifestMod>
      def fetch_manifest(download_link:, slug:, revision:) = raise(NotImplementedError)
    end
  end
end
