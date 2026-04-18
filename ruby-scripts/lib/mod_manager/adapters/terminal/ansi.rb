# frozen_string_literal: true

require "core/format"

module ModManager
  module Adapters
    module Terminal
      class Ansi
        def initialize(out = $stdout, inp = $stdin)
          @out = out
          @inp = inp
        end

        def info(msg)    = @out.puts(msg)
        def dim(msg)     = @out.puts(tty? ? "#{Core::Format::DIM}#{msg}#{Core::Format::RESET}" : msg)
        def warn(msg)    = @out.puts(tty? ? "#{Core::Format::YELLOW}#{msg}#{Core::Format::RESET}" : msg)
        def error(msg)   = @out.puts(tty? ? "#{Core::Format::RED}#{msg}#{Core::Format::RESET}" : msg)
        def success(msg) = @out.puts(tty? ? "#{Core::Format::GREEN}#{msg}#{Core::Format::RESET}" : msg)
        def bold(str)    = tty? ? "#{Core::Format::BOLD}#{str}#{Core::Format::RESET}" : str
        def muted(str)   = tty? ? "#{Core::Format::DIM}#{str}#{Core::Format::RESET}" : str

        def confirm(prompt)
          @out.print "#{prompt} [y/N] "
          @out.flush
          @inp.gets&.chomp&.downcase == "y"
        end

        private

        def tty? = @out.respond_to?(:tty?) && @out.tty? && !ENV.key?("NO_COLOR")
      end
    end
  end
end
