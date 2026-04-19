# frozen_string_literal: true

require "mod_manager/errors"
require "mod_manager/modset"

module ModManager
  module ModsetEditor
    def self.add(modset_name, collection_names, catalog)
      collection_names = Array(collection_names)
      collection_names.each do |name|
        raise Error, "#{name}: collection not found\n  run `mod collection list` to see available collections" unless catalog.collection_exist?(name)
      end
      ms = catalog.read_modset(modset_name)
      new_cols = ms.collections | collection_names
      catalog.write_modset(Modset.new(game: ms.game, collections: new_cols, mods: ms.mods, checks: ms.checks, path: ms.path))
    end

    def self.remove(modset_name, collection_name, catalog)
      ms = catalog.read_modset(modset_name)
      catalog.write_modset(Modset.new(game: ms.game, collections: ms.collections.reject { it == collection_name }, mods: ms.mods, checks: ms.checks, path: ms.path))
    end
  end
end
