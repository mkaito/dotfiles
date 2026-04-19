# frozen_string_literal: true

module Core
  module Format
    RESET = "\e[0m"
    BOLD = "\e[1m"
    DIM = "\e[2m"
    RED = "\e[31m"
    GREEN = "\e[32m"
    YELLOW = "\e[33m"

    def self.tty? = $stdout.tty? && !ENV.key?("NO_COLOR")

    def self.red(str) = tty? ? "#{RED}#{str}#{RESET}" : str
    def self.green(str) = tty? ? "#{GREEN}#{str}#{RESET}" : str
    def self.yellow(str) = tty? ? "#{YELLOW}#{str}#{RESET}" : str
    def self.bold(str) = tty? ? "#{BOLD}#{str}#{RESET}" : str
    def self.dim(str) = tty? ? "#{DIM}#{str}#{RESET}" : str
  end
end
