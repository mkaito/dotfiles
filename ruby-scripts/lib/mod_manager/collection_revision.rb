# frozen_string_literal: true

module ModManager
  # One mod entry inside a remote collection revision (provider-agnostic).
  # file_name/file_version/mod_name are optional — populated when the GraphQL
  # query fetches extended ModFile fields (e.g. for --info display).
  CollectionRevisionMod = Data.define(:mod_id, :file_id, :file_name, :file_version, :mod_name) do
    def initialize(mod_id:, file_id:, file_name: nil, file_version: nil, mod_name: nil) = super

    # Predicted archive slug — matches Adapters::Download::Nexus#fetch construction.
    # Returns nil if file_name/file_version are absent.
    def predicted_slug
      return nil unless file_name && file_version
      base = file_name.downcase
        .sub(/\s+#{Regexp.escape(file_version)}\z/i, "")
        .gsub(/[^a-z0-9]+/, "-")
        .gsub(/\A-+|-+\z/, "")
      "nexus-#{mod_id}-#{file_id}-#{base}-#{file_version}"
    end
  end

  # Metadata + mod list for one revision of a remote collection.
  CollectionRevision = Data.define(:collection_id, :collection_name, :revision_number, :mods, :download_link) do
    def initialize(collection_id:, collection_name:, revision_number:, mods:, download_link: nil) = super
  end

  # Summary row used for --list: one entry per revision.
  CollectionRevisionSummary = Data.define(:revision_number, :created_at, :mod_count, :status)
end
