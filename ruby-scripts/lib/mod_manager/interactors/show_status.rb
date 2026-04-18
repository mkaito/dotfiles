# frozen_string_literal: true

module ModManager
  module Interactors
    class ShowStatus
      def initialize(deploy:, terminal:)
        @deploy   = deploy
        @terminal = terminal
      end

      def call
        st = @deploy.status

        if st.empty?
          @terminal.dim("no active mods")
          return
        end

        st.each do |slug_ver, data|
          @terminal.info("#{slug_ver}: #{data[:links].size} link(s)")
          if data[:broken].any?
            @terminal.error("  #{data[:broken].size} broken")
            data[:broken].each { @terminal.error("  broken: #{_1}") }
          end
        end
      end
    end
  end
end
