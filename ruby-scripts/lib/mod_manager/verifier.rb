# frozen_string_literal: true

module ModManager
  module Verifier
    Check = Struct.new(:path, :type, keyword_init: true)

    def self.collect(config, modset, collections)
      (Array(config.checks) + Array(modset.checks) + collections.flat_map { Array(_1.checks) })
        .map { Check.new(path: _1["path"], type: _1["type"]) }
    end

    def self.run(checks, game_dir)
      checks.filter_map do |c|
        full = File.join(game_dir, c.path)
        case c.type
        when "dir"  then "missing dir: #{c.path}"  unless File.directory?(full)
        when "file" then "missing file: #{c.path}" unless File.file?(full)
        else "unknown check type: #{c.type}"
        end
      end
    end
  end
end
