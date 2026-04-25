# frozen_string_literal: true

require "mod_manager/services/resolver"
require "mod_manager/services/checker"
require "mod_manager/errors"

module ModManager
  module Interactors
    class LaunchGame
      def initialize(catalog:, archive:, deploy:, runtime_launch:, terminal:)
        @catalog = catalog
        @archive = archive
        @deploy = deploy
        @runtime_launch = runtime_launch
        @terminal = terminal
      end

      def call(modset_name, command:)
        ms = @catalog.read_modset(modset_name)
        collections = ms.collections.each_with_object({}) { |n, h| h[n] = @catalog.read_collection(n) }

        check_errors = Services::Checker.check_modset(ms, collections, @archive)
        raise ValidationError.new(check_errors) if check_errors.any?

        mods = Services::Resolver.resolve(ms, collections, @archive)
        raise Error, "modset has no mods" if mods.empty?

        deployed = @deploy.status
        @terminal.warn("link farm deployed — mods may overlap") if deployed.any?

        @terminal.info("launching #{modset_name} (#{mods.size} mods)")
        result = @runtime_launch.launch(mods:, modset: modset_name, command:)
        @terminal.info("exited (#{result[:exit_code]})")
        result
      end
    end
  end
end
