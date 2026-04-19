# frozen_string_literal: true

module ModManager
  module Adapters
    module Deploy
      class Memory
        attr_reader :links

        def initialize
          @links = {}
        end

        def deploy(mods:, modset: nil)
          mods.each do |mod|
            mod.files.each do |src|
              rel = src.delete_prefix("#{mod.path}/")
              @links[rel] = src
            end
          end
          {created: @links.size}
        end

        def undeploy
          count = @links.size
          @links = {}
          {removed: count}
        end

        def status
          result = Hash.new { |h, k| h[k] = {links: [], broken: []} }
          @links.each do |dst, _src|
            slug_ver = dst.split("/").first(2).join("/")
            result[slug_ver][:links] << dst
          end
          result
        end

        def path_present?(_rel_path, _type) = true
      end
    end
  end
end
