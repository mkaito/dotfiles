# frozen_string_literal: true

module ModManager
  module Adapters
    module RuntimeLaunch
      class Memory
        attr_reader :launches

        def initialize
          @launches = []
          @exit_code = 0
        end

        def stub_exit_code(code)
          @exit_code = code
        end

        def launch(mods:, modset:, command:)
          @launches << {mods:, modset:, command:}
          {exit_code: @exit_code}
        end
      end
    end
  end
end
