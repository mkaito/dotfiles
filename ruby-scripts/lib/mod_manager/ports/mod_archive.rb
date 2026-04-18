# frozen_string_literal: true

module ModManager
  module Ports
    # Browse and manage the mod archive.
    module ModArchive
      def all                    = raise(NotImplementedError)   # → Array<Mod>
      def include?(slug)         = raise(NotImplementedError)   # → bool
      def latest(slug)           = raise(NotImplementedError)   # → Mod | nil
      def install(unpacked_mod:) = raise(NotImplementedError)   # → Mod
      def delete(mod)            = raise(NotImplementedError)
      def invalidate             = raise(NotImplementedError)
    end
  end
end
