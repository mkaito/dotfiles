# frozen_string_literal: true

require "mod_manager/services/resolver"
require "mod_manager/services/checker"
require "mod_manager/services/verifier"
require "mod_manager/errors"

module ModManager
  module Interactors
    class DeployModset
      def initialize(catalog:, archive:, deploy:, terminal:)
        @catalog = catalog
        @archive = archive
        @deploy = deploy
        @terminal = terminal
      end

      def call(name, raw_checks: [])
        ms = @catalog.read_modset(name)
        collections = ms.collections.each_with_object({}) { |n, h| h[n] = @catalog.read_collection(n) }

        check_errors = Services::Checker.check_modset(ms, collections, @archive)
        raise ValidationError.new(check_errors) if check_errors.any?

        mods, conflicts = Services::Resolver.resolve(ms, collections, @archive)
        raise Error, "modset has no mods to deploy" if mods.empty?

        conflicts.each do |key, slugs|
          @terminal.warn("conflict: #{key} — #{slugs.uniq.first} overridden by #{slugs.uniq.last}")
        end

        result = @deploy.deploy(mods:, modset: name)
        @terminal.success("deployed #{result[:created]} file(s) from #{mods.size} mod(s)")

        if raw_checks.any?
          checks = raw_checks.map do |c|
            Services::Verifier::Check.new(path: c["path"], type: c["type"], present: @deploy.path_present?(c["path"], c["type"]))
          end
          vfails = Services::Verifier.run(checks)
          if vfails.any?
            @terminal.warn("verify warnings (#{vfails.size}):")
            vfails.each { @terminal.warn("  #{it}") }
          end
        end
      end
    end
  end
end
