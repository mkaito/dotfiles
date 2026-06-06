# frozen_string_literal: true

require "json"
require "fileutils"
require "core/errors"
require "core/file_io"

# The unified launch-options backup: a flat JSON object { appid => options }.
# Stdlib only.
module Steam
  module Backup
    module_function

    def load(path)
      raise Core::Error, "backup not found: #{path}" unless File.exist?(path)
      JSON.parse(File.read(path))
    rescue JSON::ParserError => e
      raise Core::Error, "backup is not valid JSON (#{path}): #{e.message}"
    end

    def save(path, map)
      FileUtils.mkdir_p(File.dirname(path))
      Core::FileIO.atomic_write(path, "#{JSON.pretty_generate(map.sort.to_h)}\n")
    end
  end
end
