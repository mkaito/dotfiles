# frozen_string_literal: true

module Steam
  module VdfNav
    def self.dig(hash, *keys)
      keys.reduce(hash) do |h, k|
        break nil unless h.is_a?(Hash)
        _, val = h.find { |hk, _| hk.casecmp?(k.to_s) }
        val
      end
    end

    def self.set(hash, *keys, value:)
      *path, last = keys
      node = path.reduce(hash) do |h, k|
        existing = h.keys.find { |hk| hk.casecmp?(k.to_s) } || k.to_s
        h[existing] ||= {}
      end
      existing_key = node.keys.find { |hk| hk.casecmp?(last.to_s) } || last.to_s
      node[existing_key] = value
    end
  end
end
