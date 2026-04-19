# frozen_string_literal: true

module ModManager
  module Adapters
    module ModsetState
      class Memory
        attr_reader :wiped

        def initialize
          @state = {}
          @wiped = []
        end

        def seed(modset, redirect: [], overlay: [])
          @state[modset] = {redirect:, overlay:}
        end

        def summary(modset:)
          data = @state[modset]
          return nil unless data
          {
            redirect_bytes: data[:redirect].sum { _1[:bytes] },
            overlay_bytes:  data[:overlay].sum { _1[:bytes] }
          }
        end

        def details(modset:)
          data = @state[modset] || {redirect: [], overlay: []}
          data.transform_values { |files| files.sort_by { |f| f[:path] } }
        end

        def wipe(modset:)
          data = @state.delete(modset)
          bytes = data ? (data[:redirect] + data[:overlay]).sum { _1[:bytes] } : 0
          @wiped << modset
          {bytes_freed: bytes}
        end
      end
    end
  end
end
