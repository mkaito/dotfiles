# frozen_string_literal: true

module ModManager
  module Adapters
    module Terminal
      class Memory
        attr_reader :lines

        def initialize
          @lines = []
          @confirm_responses = []
        end

        def info(msg) = (@lines << {level: :info, msg: msg.to_s})
        def dim(msg) = (@lines << {level: :dim, msg: msg.to_s})
        def warn(msg) = (@lines << {level: :warn, msg: msg.to_s})
        def error(msg) = (@lines << {level: :error, msg: msg.to_s})
        def success(msg) = (@lines << {level: :success, msg: msg.to_s})
        def bold(str) = str.to_s
        def muted(str) = str.to_s
        def green(str) = str.to_s

        def confirm(_prompt) = @confirm_responses.shift || false

        # Test helpers

        def stub_confirm(*responses)
          @confirm_responses.concat(responses)
        end

        def output
          @lines.map { it[:msg] }.join("\n")
        end

        def warnings = @lines.select { it[:level] == :warn }.map { it[:msg] }
        def errors = @lines.select { it[:level] == :error }.map { it[:msg] }
        def infos = @lines.select { it[:level] == :info }.map { it[:msg] }
      end
    end
  end
end
