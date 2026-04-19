# frozen_string_literal: true

require "core/format"
require "mod_manager/errors"

module ModManager
  module Interactors
    class StateCrud
      def initialize(catalog:, modset_state:, terminal:)
        @catalog       = catalog
        @modset_state  = modset_state
        @terminal      = terminal
      end

      def show(modset_name)
        raise Error, "modset not found: #{modset_name}" unless @catalog.modset_exist?(modset_name)

        data = @modset_state.details(modset: modset_name)

        if data[:redirect].empty? && data[:overlay].empty?
          @terminal.info("no state for #{modset_name}")
          return
        end

        redirect_bytes = data[:redirect].sum { _1[:bytes] }
        overlay_bytes  = data[:overlay].sum { _1[:bytes] }

        unless data[:redirect].empty?
          @terminal.info("redirects (link farm)   #{Core::Format.bytes(redirect_bytes)}")
          data[:redirect].each { @terminal.info("  #{_1[:path]}    #{Core::Format.bytes(_1[:bytes])}") }
        end

        unless data[:overlay].empty?
          @terminal.info("overlay (upper)   #{Core::Format.bytes(overlay_bytes)}")
          data[:overlay].each { @terminal.info("  #{_1[:path]}    #{Core::Format.bytes(_1[:bytes])}") }
        end

        @terminal.info("total   #{Core::Format.bytes(redirect_bytes + overlay_bytes)}")
      end

      def clean(modset_name, yes: false)
        raise Error, "modset not found: #{modset_name}" unless @catalog.modset_exist?(modset_name)

        summary = @modset_state.summary(modset: modset_name)
        total = summary ? summary[:redirect_bytes] + summary[:overlay_bytes] : 0

        if total.zero?
          @terminal.info("no state for #{modset_name}")
          return
        end

        unless yes || @terminal.confirm("delete #{Core::Format.bytes(total)} of state for #{modset_name}?")
          @terminal.info("aborted")
          return
        end

        result = @modset_state.wipe(modset: modset_name)
        @terminal.success("wiped #{Core::Format.bytes(result[:bytes_freed])}")
      end
    end
  end
end
