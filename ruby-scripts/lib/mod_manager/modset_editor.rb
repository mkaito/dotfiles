# frozen_string_literal: true

require "toml-rb"
require_relative "errors"
require_relative "modset"
require_relative "../core/file_io"

module ModManager
  module ModsetEditor
    def self.add(modset_path, collection_names, config)
      collection_names = Array(collection_names)
      collection_names.each do |name|
        path = File.join(config.collections_dir, "#{name}.toml")
        raise Error, "#{name}: collection not found\n  run `mod collection list` to see available collections" unless File.exist?(path)
      end
      rs = Modset.load(modset_path)
      new_cols = rs.collections | collection_names
      write_modset(modset_path, rs, new_cols)
    end

    def self.remove(modset_path, collection_name)
      rs = Modset.load(modset_path)
      write_modset(modset_path, rs, rs.collections.reject { _1 == collection_name })
    end

    private_class_method def self.write_modset(path, rs, new_cols)
      Core::FileIO.atomic_write(path, TomlRB.dump(
        "game"        => rs.game,
        "collections" => new_cols,
        "mods"        => rs.mods
      ))
    end
  end
end
