# frozen_string_literal: true

module ModManager
  module Ports
    module ModsetState
      def summary(modset:) = raise(NotImplementedError)
      def details(modset:) = raise(NotImplementedError)
      def wipe(modset:)    = raise(NotImplementedError)
    end
  end
end
