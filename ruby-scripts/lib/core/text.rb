# frozen_string_literal: true

module Core
  module Text
    def self.slugify(name)
      name.downcase.gsub(/[^a-z0-9]+/, "-").gsub(/\A-+|-+\z/, "")
    end
  end
end
