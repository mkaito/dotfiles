# frozen_string_literal: true

module Core
  module CLI
    def self.confirm(prompt, stdin: $stdin, stdout: $stdout)
      stdout.print "#{prompt} [y/N] "
      stdout.flush
      stdin.gets&.chomp&.downcase == "y"
    end
  end
end
