# frozen_string_literal: true

require "fileutils"
require "rubygems/version"
require "toml-rb"
require "mod_manager/mod"
require "mod_manager/log"
require "mod_manager/errors"
require "core/file_io"

module ModManager
  module Adapters
    module ModArchive
      class Filesystem
        def initialize(dir)
          @dir = dir
        end

        def all
          @all ||= load_all
        end

        def include?(slug)
          all.any? { _1.slug == slug }
        end

        def latest(slug)
          all.select { _1.slug == slug }
            .max_by { version_tuple(_1.version) }
        end

        def install(unpacked_mod:)
          dest = File.join(@dir, unpacked_mod.game, unpacked_mod.slug)
          FileUtils.mkdir_p(dest)
          Dir.glob("#{unpacked_mod.tmp_dir}/*").each { FileUtils.cp_r(_1, dest) }
          meta = {
            "name"    => unpacked_mod.name,
            "slug"    => unpacked_mod.slug,
            "version" => unpacked_mod.version,
            "game"    => unpacked_mod.game,
            "depends" => [],
            "source"  => unpacked_mod.source,
          }
          Core::FileIO.atomic_write(File.join(dest, "meta.toml"), TomlRB.dump(meta))
          invalidate
          Mod.load(dest)
        end

        def delete(mod)
          FileUtils.rm_rf(mod.path)
          parent = File.dirname(mod.path)
          Dir.rmdir(parent) if Dir.empty?(parent)
          invalidate
        end

        def invalidate
          @all = nil
        end

        private

        def load_all
          return [] unless File.directory?(@dir)
          Dir.glob("#{@dir}/*/*/meta.toml").filter_map do |meta|
            Mod.load(File.dirname(meta))
          rescue ModManager::Error, TomlRB::ParseError, Errno::ENOENT, Errno::EACCES => e
            Log.warn("skipping #{meta}: #{e.message}")
            nil
          end
        end

        def version_tuple(v)
          Gem::Version.new(v)
        rescue ArgumentError
          Gem::Version.new("0")
        end
      end
    end
  end
end
