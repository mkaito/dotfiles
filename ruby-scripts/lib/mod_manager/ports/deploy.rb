# frozen_string_literal: true

module ModManager
  module Ports
    # Deploy mods to the game directory. Implementation decides the mechanism
    # (symlink farm, overlayfs, copy, etc.) without the core knowing or caring.
    module Deploy
      def deploy(mods:)              = raise(NotImplementedError)   # → { created: N }
      def undeploy                   = raise(NotImplementedError)   # → { removed: N }
      def status                     = raise(NotImplementedError)   # → Array<{ slug_ver:, links:, broken: }>
      def path_present?(rel_path, type) = raise(NotImplementedError)  # → bool
    end
  end
end
