# frozen_string_literal: true

module ModManager
  module Log
    LEVELS = { "debug" => 0, "info" => 1, "warn" => 2, "error" => 3 }.freeze

    class << self
      def debug(msg) = log(0, "DEBUG", msg)
      def info(msg)  = log(1, "INFO",  msg)
      def warn(msg)  = log(2, "WARN",  msg)
      def error(msg) = log(3, "ERROR", msg)

      private

      def level
        LEVELS.fetch(ENV.fetch("RUBY_LOG_LEVEL", "info").downcase, 1)
      end

      def log(lvl, label, msg)
        puts "[#{label}] #{msg}" if lvl >= level
      end
    end
  end
end
