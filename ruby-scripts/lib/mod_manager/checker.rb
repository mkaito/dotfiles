# frozen_string_literal: true

require_relative "atom"
require_relative "collection"
require_relative "modset"
require_relative "topo_sort"

module ModManager
  class Checker
    def initialize(archive, config)
      @archive = archive
      @config  = config
    end

    def check_all
      errors = []

      Dir.glob("#{@config.collections_dir}/*.toml").each do |path|
        col = Collection.load(path)
        errors.concat(check_collection(col))
      rescue ValidationError => e
        errors.concat(e.errors)
      end

      Dir.glob("#{@config.modsets_dir}/*.toml").each do |path|
        errors.concat(check_modset(Modset.load(path)))
      rescue ValidationError => e
        errors.concat(e.errors)
      end

      errors
    end

    def check_modset(modset)
      errors  = []
      resolved = []

      modset.collections.each do |name|
        path = File.join(@config.collections_dir, "#{name}.toml")
        unless File.exist?(path)
          errors << "collection not found: #{path}"
          next
        end
        col = Collection.load(path)
        errors.concat(check_collection(col))
        col.mods.each do |atom_str|
          mod = Atom.resolve(atom_str, @archive)
          resolved << mod if mod
        end
      end

      modset.mods.each do |atom_str|
        mod = Atom.resolve(atom_str, @archive)
        errors << "unresolved atom: #{atom_str}" unless mod
        resolved << mod if mod
      end

      resolved.each do |mod|
        mod.depends.each do |dep|
          dep_slug = Atom.parse(dep)[:slug]
          errors << "#{mod}: missing dep #{dep}" unless @archive.include?(dep_slug)
        end
      end

      begin
        TopoSort.sort(resolved)
      rescue TopoSort::CycleError => e
        errors << "dependency cycle: #{e.message}"
      end

      errors
    end

    def check_collection(col)
      col.mods.filter_map do |atom_str|
        "#{col.path}: unresolved atom #{atom_str}" unless Atom.resolve(atom_str, @archive)
      end
    end
  end
end
