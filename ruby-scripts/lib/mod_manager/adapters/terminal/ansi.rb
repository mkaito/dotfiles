# frozen_string_literal: true

module ModManager
  module Adapters
    module Terminal
      class Ansi
        RESET  = "\e[0m"
        BOLD   = "\e[1m"
        DIM    = "\e[2m"
        RED    = "\e[31m"
        GREEN  = "\e[32m"
        YELLOW = "\e[33m"

        def initialize(out = $stdout, inp = $stdin)
          @out = out
          @inp = inp
        end

        def info(msg)    = @out.puts(msg)
        def dim(msg)     = @out.puts(tty? ? "#{DIM}#{msg}#{RESET}" : msg)
        def warn(msg)    = @out.puts(tty? ? "#{YELLOW}#{msg}#{RESET}" : msg)
        def error(msg)   = @out.puts(tty? ? "#{RED}#{msg}#{RESET}" : msg)
        def success(msg) = @out.puts(tty? ? "#{GREEN}#{msg}#{RESET}" : msg)
        def bold(str)    = tty? ? "#{BOLD}#{str}#{RESET}" : str
        def muted(str)   = tty? ? "#{DIM}#{str}#{RESET}" : str

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
