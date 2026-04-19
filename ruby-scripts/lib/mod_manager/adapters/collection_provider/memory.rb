# frozen_string_literal: true

require "mod_manager/errors"

module ModManager
  module Adapters
    module CollectionProvider
      class Memory
        def initialize
          @revisions = {}  # slug → CollectionRevision
          @revision_lists = {}  # slug → Array<CollectionRevisionSummary>
          @manifests = {}  # slug → Array<CollectionManifestMod>
        end

        def stub_revision(slug, revision)
          @revisions[slug] = revision
        end

        def stub_revision_list(slug, summaries)
          @revision_lists[slug] = summaries
        end

        def stub_manifest(slug, mods)
          @manifests[slug] = mods
        end

        def fetch_revision(slug:, revision: nil)
          @revisions.fetch(slug) { raise Core::Error, "no stub revision for #{slug}" }
        end

        def list_revisions(slug:)
          @revision_lists.fetch(slug) { raise Core::Error, "no stub revision list for #{slug}" }
        end

        def fetch_manifest(download_link:, slug:, revision:)
          @manifests.fetch(slug) { raise Core::Error, "no stub manifest for #{slug}" }
        end
      end
    end
  end
end
