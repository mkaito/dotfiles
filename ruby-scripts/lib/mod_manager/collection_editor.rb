# frozen_string_literal: true

require "toml-rb"
require_relative "errors"
require_relative "atom"
require_relative "collection"
require_relative "log"

module ModManager
  module CollectionEditor
    def self.add(collection_path, atom_str, archive)
      col = Collection.load(collection_path)
      mod = Atom.resolve(atom_str, archive) or raise Error, "unresolved: #{atom_str}"
      existing_slugs = col.mods.map { Atom.parse(_1)[:slug] }.to_set
      deps     = transitive_deps(mod, archive)
      new_mods = (deps + [mod.slug]).reject { existing_slugs.include?(_1) }
      return if new_mods.empty?
      write_collection(collection_path, col, col.mods + new_mods)
    end

    def self.remove(collection_path, slug, archive)
      col = Collection.load(collection_path)
      dependents = col.mods.filter_map do |atom_str|
        m = Atom.resolve(atom_str, archive)
        next unless m && m.depends.any? { Atom.parse(_1)[:slug] == slug }
        m.slug
      end
      Log.warn("#{slug} is depended on by: #{dependents.join(", ")}") if dependents.any?
      new_mods = col.mods.reject { Atom.parse(_1)[:slug] == slug }
      write_collection(collection_path, col, new_mods)
    end

    private_class_method def self.transitive_deps(mod, archive, seen = Set.new)
      result = []
      mod.depends.each do |dep_atom|
        dep_slug = Atom.parse(dep_atom)[:slug]
        next if seen.include?(dep_slug)
        seen << dep_slug
        dep = Atom.resolve(dep_atom, archive) or raise Error, "unresolved dependency: #{dep_atom}"
        result.concat(transitive_deps(dep, archive, seen))
        result << dep.slug
      end
      result
    end

    private_class_method def self.write_collection(path, col, new_mods)
      data = { "name" => col.name, "game" => col.game, "mods" => new_mods }
      data["checks"] = col.checks if col.checks.any?
      tmp = "#{path}.tmp.#{Process.pid}"
      File.write(tmp, TomlRB.dump(data))
      File.rename(tmp, path)
    end
  end
end
