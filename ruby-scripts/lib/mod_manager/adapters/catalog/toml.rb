# frozen_string_literal: true

require "toml-rb"
require "mod_manager/log"
require "mod_manager/collection"
require "mod_manager/modset"
require "mod_manager/errors"
require "core/file_io"

module ModManager
  module Adapters
    module Catalog
      # Filesystem-backed catalog. Reads/writes TOML files in XDG config dirs.
      # Calls `chezmoi add` after every write so dotfiles stay in sync.
      class Toml
        def initialize(collections_dir, modsets_dir)
          @col_dir = File.expand_path(collections_dir)
          @ms_dir = File.expand_path(modsets_dir)
        end

        # ── collections ──────────────────────────────────────────────────────

        def list_collections
          Dir.glob("#{@col_dir}/*.toml").sort.map { File.basename(it, ".toml") }
        end

        def collection_exist?(name) = File.exist?(col_path(name))

        def read_collection(name)
          raise Error, "collection not found: #{name}" unless collection_exist?(name)
          Collection.load(col_path(name))
        end

        def write_collection(col)
          Core::FileIO.atomic_write(col_path(col.name), TomlRB.dump("name" => col.name, "mods" => col.mods))
          chezmoi_add(col_path(col.name))
        end

        def delete_collection(name)
          raise Error, "collection not found: #{name}" unless collection_exist?(name)
          path = col_path(name)
          File.delete(path)
          chezmoi_forget(path)
        end

        # ── modsets ───────────────────────────────────────────────────────────

        def list_modsets
          Dir.glob("#{@ms_dir}/*.toml").sort.map { File.basename(it, ".toml") }
        end

        def modset_exist?(name) = File.exist?(ms_path(name))

        def read_modset(name)
          raise Error, "modset not found: #{name}" unless modset_exist?(name)
          Modset.load(ms_path(name))
        end

        def write_modset(ms)
          name = File.basename(ms.path, ".toml")
          Core::FileIO.atomic_write(ms_path(name), TomlRB.dump(
            "game" => ms.game,
            "collections" => ms.collections,
            "mods" => ms.mods
          ))
          chezmoi_add(ms_path(name))
        end

        def delete_modset(name)
          raise Error, "modset not found: #{name}" unless modset_exist?(name)
          path = ms_path(name)
          File.delete(path)
          chezmoi_forget(path)
        end

        private

        def col_path(name) = File.join(@col_dir, "#{name}.toml")
        def ms_path(name) = File.join(@ms_dir, "#{name}.toml")

        def chezmoi_add(path)
          ok = system("chezmoi", "add", path, out: File::NULL, err: File::NULL)
          Log.warn("chezmoi add #{path} failed") unless ok
        end

        def chezmoi_forget(path)
          ok = system("chezmoi", "forget", "--force", path, out: File::NULL, err: File::NULL)
          Log.warn("chezmoi forget #{path} failed") unless ok
        end
      end
    end
  end
end
