# frozen_string_literal: true

require "mod_manager/services/checker"
require "mod_manager/errors"

module ModManager
  module Interactors
    class VerifyCatalog
      def initialize(catalog:, archive:, terminal:)
        @catalog = catalog
        @archive = archive
        @terminal = terminal
      end

      def call(names:, mode:)
        if mode == :all
          ok1 = call(names: [], mode: :collections)
          ok2 = call(names: [], mode: :modsets)
          return ok1 && ok2
        end

        targets = names.empty? ? list_all(mode) : names
        all_ok = true

        targets.each do |name|
          errors = check(name, mode)
          if errors.empty?
            @terminal.info("#{name}: ok")
          else
            all_ok = false
            errors.each { @terminal.error("  #{name}: #{it}") }
          end
        rescue Error => e
          all_ok = false
          @terminal.error("  #{name}: #{e.message}")
        end

        all_ok
      end

      private

      def list_all(mode)
        case mode
        when :collections then @catalog.list_collections
        when :modsets then @catalog.list_modsets
        end
      end

      def check(name, mode)
        case mode
        when :collections
          raise Error, "collection not found: #{name}" unless @catalog.collection_exist?(name)
          Services::Checker.check_collection(@catalog.read_collection(name), @archive)
        when :modsets
          raise Error, "modset not found: #{name}" unless @catalog.modset_exist?(name)
          ms = @catalog.read_modset(name)
          cols = ms.collections.each_with_object({}) do |n, h|
            h[n] = @catalog.read_collection(n) if @catalog.collection_exist?(n)
          end
          Services::Checker.check_modset(ms, cols, @archive)
        end
      end
    end
  end
end
