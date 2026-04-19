# frozen_string_literal: true

module ModManager
  module Ports
    # Fetch a mod from an external provider. Returns core models; all provider
    # specifics (HTTP, auth, archive format) stay inside the adapter.
    module Download
      # → Array<FileInfo>
      def list_files(mod_id:) = raise(NotImplementedError)

      # Download + unpack to a temp dir. Caller passes UnpackedMod to ModArchive#install.
      # → UnpackedMod
      def fetch(mod_id:, file_id:, slug: nil) = raise(NotImplementedError)

      # Returns true if file_id exists in the mod's file list on the provider.
      def file_exist?(mod_id:, file_id:) = raise(NotImplementedError)
    end
  end
end
