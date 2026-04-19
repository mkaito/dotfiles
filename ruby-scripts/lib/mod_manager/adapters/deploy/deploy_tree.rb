# frozen_string_literal: true

require "set"

module ModManager
  module Adapters
    module Deploy
      # Partitions a flat file list into the highest-possible symlink targets.
      # Subtrees wholly owned by one mod → directory symlink.
      # Shared subtrees → real directory (mkdir at deploy time), recurse.
      #
      # Input:  [{dst_rel:, mod_path_rel:}]   one entry per file, already deduplicated
      # Output: [{src_rel:, dst_rel:, dir:}]  symlinks to create
      module DeployTree
        Node = Struct.new(:children, :files, :mods, keyword_init: true)
        private_constant :Node

        def self.solve(entries)
          root = build_tree(entries)
          walk(root, "")
        end

        class << self
          private

          def build_tree(entries)
            root = Node.new(children: {}, files: [], mods: Set.new)
            entries.each do |entry|
              parts = entry[:dst_rel].split("/")
              insert(root, parts, entry[:mod_path_rel])
            end
            root
          end

          def insert(node, parts, mod_path_rel)
            node.mods << mod_path_rel
            if parts.size == 1
              node.files << { name: parts[0], mod_path_rel: }
            else
              child = node.children[parts[0]] ||= Node.new(children: {}, files: [], mods: Set.new)
              insert(child, parts[1..], mod_path_rel)
            end
          end

          def walk(node, prefix)
            results = []

            node.files.each do |f|
              dst_rel = prefix.empty? ? f[:name] : "#{prefix}/#{f[:name]}"
              results << { src_rel: "#{f[:mod_path_rel]}/#{dst_rel}", dst_rel:, dir: false }
            end

            node.children.each do |name, child|
              child_prefix = prefix.empty? ? name : "#{prefix}/#{name}"
              if child.mods.size == 1
                mod_path_rel = child.mods.first
                results << { src_rel: "#{mod_path_rel}/#{child_prefix}", dst_rel: child_prefix, dir: true }
              else
                results.concat(walk(child, child_prefix))
              end
            end

            results
          end
        end
      end
    end
  end
end
