# frozen_string_literal: true

module ModManager
  module Ports
    # Read/write collections and modsets. Identified by name; adapters handle paths.
    module Catalog
      def list_collections           = raise(NotImplementedError)   # → Array<String>
      def collection_exist?(name)    = raise(NotImplementedError)   # → bool
      def read_collection(name)      = raise(NotImplementedError)   # → Collection
      def write_collection(col)      = raise(NotImplementedError)   # col: Collection
      def delete_collection(name)    = raise(NotImplementedError)

      def list_modsets               = raise(NotImplementedError)   # → Array<String>
      def modset_exist?(name)        = raise(NotImplementedError)   # → bool
      def read_modset(name)          = raise(NotImplementedError)   # → Modset
      def write_modset(ms)           = raise(NotImplementedError)   # ms: Modset
      def delete_modset(name)        = raise(NotImplementedError)
    end
  end
end
