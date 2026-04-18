# frozen_string_literal: true

require "toml-rb"
require_relative "mod"
require_relative "log"

module ModManager
  class Archive
    def initialize(dir)
      @dir = dir
    end

    def all
      @all ||= load_all
    end

    def versions(slug)
      all.select { _1.slug == slug }.sort_by { version_tuple(_1.version) }.reverse
    end

    def latest(slug)
      versions(slug).first
    end

    def include?(slug)
      all.any? { _1.slug == slug }
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
      v.to_s.split(".").map(&:to_i)
    end
  end
end
