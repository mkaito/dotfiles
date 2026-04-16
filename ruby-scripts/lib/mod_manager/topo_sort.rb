# frozen_string_literal: true

require "set"
require_relative "atom"

module ModManager
  module TopoSort
    CycleError = Class.new(StandardError)

    def self.sort(mods)
      slugs     = mods.map(&:slug).to_set
      in_degree = mods.each_with_object({}) { |m, h| h[m.slug] = 0 }
      adj       = mods.each_with_object({}) { |m, h| h[m.slug] = [] }

      mods.each do |mod|
        mod.depends.each do |dep_atom|
          dep = Atom.parse(dep_atom)[:slug]
          next unless slugs.include?(dep)
          adj[dep] << mod.slug
          in_degree[mod.slug] += 1
        end
      end

      queue   = in_degree.filter_map { |slug, d| slug if d.zero? }
      by_slug = mods.each_with_object({}) { |m, h| h[m.slug] = m }
      result  = []

      until queue.empty?
        slug = queue.shift
        result << by_slug[slug]
        adj[slug].each do |dep|
          in_degree[dep] -= 1
          queue << dep if in_degree[dep].zero?
        end
      end

      if result.size < mods.size
        remaining = mods.map(&:slug) - result.map(&:slug)
        raise CycleError, "cycle among: #{remaining.join(", ")}"
      end

      result
    end
  end
end
