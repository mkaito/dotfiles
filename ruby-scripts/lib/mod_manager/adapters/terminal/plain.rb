# frozen_string_literal: true

module ModManager
  module Adapters
    module Terminal
      class Plain
        def initialize(out = $stdout, inp = $stdin)
          @out = out
          @inp = inp
        end

        def info(msg)    = @out.puts(msg)
        def dim(msg)     = @out.puts(msg)
        def warn(msg)    = @out.puts(msg)
        def error(msg)   = @out.puts(msg)
        def success(msg) = @out.puts(msg)
        def bold(str)    = str
        def muted(str)   = str

        def confirm(prompt)
          @out.print "#{prompt} [y/N] "
          @out.flush
          @inp.gets&.chomp&.downcase == "y"
        end
      end
    end
  end
end
