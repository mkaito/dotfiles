# frozen_string_literal: true

module ModManager
  module Interactors
    class ResetDeploy
      def initialize(deploy:, terminal:)
        @deploy = deploy
        @terminal = terminal
      end

      def call(yes: false)
        st = @deploy.status
        total = st.values.sum { it[:links].size + it[:broken].size }

        if total.zero?
          @terminal.info("no active symlinks")
          return
        end

        @terminal.info("#{total} symlink(s) to remove")
        unless yes || @terminal.confirm("proceed?")
          @terminal.info("aborted")
          return
        end

        @terminal.success("removed #{@deploy.undeploy[:removed]} symlink(s)")
      end
    end
  end
end
