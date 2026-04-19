# frozen_string_literal: true

module ModManager
  module Services
    module Verifier
      Check = Struct.new(:path, :type, :present, keyword_init: true)

      def self.run(checks)
        checks.filter_map do |c|
          next if c.present
          case c.type
          when "dir" then "missing dir: #{c.path}"
          when "file" then "missing file: #{c.path}"
          else "unknown check type: #{c.type}"
          end
        end
      end
    end
  end
end
