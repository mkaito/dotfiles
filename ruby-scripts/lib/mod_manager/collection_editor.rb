# frozen_string_literal: true

require "toml-rb"
require_relative "errors"
require_relative "collection"
require_relative "../core/file_io"

module ModManager
  module CollectionEditor
    def self.add(collection_path, slugs, archive)
      slugs = Array(slugs)
      slugs.each do |slug|
        raise Error, "#{slug}: not in archive\n  run `mod list` to see available mods" unless archive.include?(slug)
      end
      col = Collection.load(collection_path)
      new_mods = col.mods | slugs
      write_collection(collection_path, col, new_mods) unless new_mods == col.mods
    end

    def self.remove(collection_path, slug)
      col = Collection.load(collection_path)
      write_collection(collection_path, col, col.mods.reject { _1 == slug })
    end

    private_class_method def self.write_collection(path, col, new_mods)
      Core::FileIO.atomic_write(path, TomlRB.dump("name" => col.name, "mods" => new_mods))
    end
  end
end
