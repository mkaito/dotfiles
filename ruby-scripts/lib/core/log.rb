# frozen_string_literal: true

require_relative "format"

module Core
  module Log
    LEVELS = { "debug" => 0, "info" => 1, "warn" => 2, "error" => 3 }.freeze

    class << self
      def debug(msg) = log(0, "DEBUG", msg)
      def info(msg)  = log(1, "INFO",  msg)
      def warn(msg)  = log(2, "WARN",  msg)
      def error(msg) = log(3, "ERROR", msg)

      private

      def level
        LEVELS.fetch(ENV.fetch("RUBY_LOG_LEVEL", "warn").downcase, 2)
      end

      def log(lvl, label, msg)
        return unless lvl >= level
        colored_label = case label
          when "WARN"  then Format.yellow("[WARN]")
          when "ERROR" then Format.red("[ERROR]")
          else "[#{label}]"
        end
        puts "#{colored_label} #{msg}"
      end
    end
  end
end
