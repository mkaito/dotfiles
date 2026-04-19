# frozen_string_literal: true

module ModManager
  module Ports
    # All user-facing output and interaction. Adapters decide formatting.
    # Core emits semantic intent; the adapter decides colour, structure, destination.
    module Terminal
      # Output methods — write one line with semantic level.
      def info(msg) = raise(NotImplementedError)
      def warn(msg) = raise(NotImplementedError)
      def error(msg) = raise(NotImplementedError)
      def success(msg) = raise(NotImplementedError)
      def dim(msg) = raise(NotImplementedError)

      # Inline formatters — return a decorated string for embedding in messages.
      def bold(str) = raise(NotImplementedError)   # → String
      def muted(str) = raise(NotImplementedError)   # → String (dim inline)

      # User interaction.
      def confirm(prompt) = raise(NotImplementedError)   # → bool
    end
  end
end
