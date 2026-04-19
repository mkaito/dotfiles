# frozen_string_literal: true

module ModManager
  module Ports
    module RuntimeLaunch
      def launch(mods:, modset:, command:) = raise(NotImplementedError)
    end
  end
end
