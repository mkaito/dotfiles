# frozen_string_literal: true

require_relative "../../errors"

module ModManager
  module Adapters
    module Download
      class Memory
        attr_reader :fetched  # Array<{mod_id:, file_id:, slug:}>

        def initialize
          @fetched     = []
          @file_stubs  = {}   # mod_id → Array<Hash>
          @fetch_stubs = {}   # [mod_id, file_id] → UnpackedMod
        end

        def stub_files(mod_id, files:)
          @file_stubs[mod_id] = files
        end

        def stub_fetch(mod_id, file_id, unpacked_mod:)
          @fetch_stubs[[mod_id, file_id]] = unpacked_mod
        end

        def list_files(mod_id:)
          @file_stubs.fetch(mod_id) { raise Core::Error, "no stub for mod_id #{mod_id}" }
        end

        def file_exist?(mod_id:, file_id:)
          @fetch_stubs.key?([mod_id, file_id])
        end

        def fetch(mod_id:, file_id:, slug: nil)
          @fetched << { mod_id:, file_id:, slug: }
          @fetch_stubs.fetch([mod_id, file_id]) { raise Core::Error, "no stub for #{mod_id}/#{file_id}" }
        end
      end
    end
  end
end
