# frozen_string_literal: true

module ModManager
  module Adapters
    module CollectionProvider
      module Translator
        def self.revision_mod(m)
          CollectionRevisionMod.new(
            mod_id: m["file"]["modId"],
            file_id: m["fileId"],
            file_name: m["file"]["name"],
            file_version: m["file"]["version"]
          )
        end

        def self.manifest_mod(m)
          CollectionManifestMod.new(
            file_id: m.dig("source", "fileId"),
            mod_id: m.dig("source", "modId"),
            name: m["name"],
            phase: m["phase"],
            choices: m["choices"]
          )
        end
      end
    end
  end
end
