# frozen_string_literal: true

module ModManager
  module Adapters
    module Terminal
      class Memory
        attr_reader :lines

        def initialize
          @lines            = []
          @confirm_responses = []
        end

        def info(msg)    = (@lines << { level: :info,    msg: msg.to_s })
        def dim(msg)     = (@lines << { level: :dim,     msg: msg.to_s })
        def warn(msg)    = (@lines << { level: :warn,    msg: msg.to_s })
        def error(msg)   = (@lines << { level: :error,   msg: msg.to_s })
        def success(msg) = (@lines << { level: :success, msg: msg.to_s })
        def bold(str)    = str.to_s
        def muted(str)   = str.to_s

        def confirm(_prompt) = @confirm_responses.shift || false

        # Test helpers

        def stub_confirm(*responses)
          @confirm_responses.concat(responses)
        end

        def output
          @lines.map { _1[:msg] }.join("\n")
        end

        def warnings = @lines.select { _1[:level] == :warn    }.map { _1[:msg] }
        def errors   = @lines.select { _1[:level] == :error   }.map { _1[:msg] }
        def infos    = @lines.select { _1[:level] == :info    }.map { _1[:msg] }
      end
    end
  end
end
