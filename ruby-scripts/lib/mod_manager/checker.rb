# frozen_string_literal: true

require_relative "collection"
require_relative "modset"

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
      errors = []

      modset.collections.each do |name|
        path = File.join(@config.collections_dir, "#{name}.toml")
        unless File.exist?(path)
          errors << "collection not found: #{path}"
          next
        end
        errors.concat(check_collection(Collection.load(path)))
      end

      modset.mods.each do |slug|
        errors << "not in archive: #{slug}" unless @archive.include?(slug)
      end

      errors
    end

    def check_collection(col)
      col.mods.filter_map do |slug|
        "#{col.path}: not in archive: #{slug}" unless @archive.include?(slug)
      end
    end
  end
end
