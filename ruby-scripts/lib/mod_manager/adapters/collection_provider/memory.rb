# frozen_string_literal: true

require_relative "../../errors"

module ModManager
  module Adapters
    module CollectionProvider
      class Memory
        def initialize
          @revisions      = {}  # slug → CollectionRevision
          @revision_lists = {}  # slug → Array<CollectionRevisionSummary>
        end

        def stub_revision(slug, revision)
          @revisions[slug] = revision
        end

        def stub_revision_list(slug, summaries)
          @revision_lists[slug] = summaries
        end

        def fetch_revision(slug:, revision: nil)
          @revisions.fetch(slug) { raise Core::Error, "no stub revision for #{slug}" }
        end

        def list_revisions(slug:)
          @revision_lists.fetch(slug) { raise Core::Error, "no stub revision list for #{slug}" }
        end
      end
    end
  end
end
