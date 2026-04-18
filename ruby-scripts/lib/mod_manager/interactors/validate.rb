# frozen_string_literal: true

require_relative "../services/checker"
require_relative "../errors"

module ModManager
  module Interactors
    class Validate
      def initialize(catalog:, archive:, terminal:)
        @catalog  = catalog
        @archive  = archive
        @terminal = terminal
      end

      def call(name = nil)
        errors = if name
          if @catalog.modset_exist?(name)
            ms   = @catalog.read_modset(name)
            cols = ms.collections.each_with_object({}) { |n, h| h[n] = @catalog.read_collection(n) }
            Services::Checker.check_modset(ms, cols, @archive)
          elsif @catalog.collection_exist?(name)
            Services::Checker.check_collection(@catalog.read_collection(name), @archive)
          else
            raise Error, "no modset or collection named #{name.inspect}"
          end
        else
          collections = @catalog.list_collections.filter_map do |n|
            @catalog.read_collection(n)
          rescue ValidationError
            nil
          end
          modsets = @catalog.list_modsets.filter_map do |n|
            @catalog.read_modset(n)
          rescue ValidationError
            nil
          end
          Services::Checker.check_all(collections, modsets, @archive)
        end

        if errors.empty?
          @terminal.success("ok")
          true
        else
          errors.each { @terminal.error("  #{_1}") }
          false
        end
      end
    end
  end
end
