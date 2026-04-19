# frozen_string_literal: true

require "mod_manager/errors"
require "mod_manager/collection"

module ModManager
  module CollectionEditor
    def self.add(collection_name, slugs, archive, catalog)
      slugs = Array(slugs)
      slugs.each do |slug|
        raise Error, "#{slug}: not in archive\n  run `mod list` to see available mods" unless archive.include?(slug)
      end
      col = catalog.read_collection(collection_name)
      new_mods = col.mods | slugs
      catalog.write_collection(Collection.new(name: col.name, mods: new_mods, path: col.path)) unless new_mods == col.mods
    end

    def self.remove(collection_name, slug, catalog)
      col = catalog.read_collection(collection_name)
      catalog.write_collection(Collection.new(name: col.name, mods: col.mods.reject { it == slug }, path: col.path))
    end
  end
end
