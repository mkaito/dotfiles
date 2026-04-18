# frozen_string_literal: true

module ModManager
  module Services
    # All inputs (collections, modsets) are already-loaded objects.
    # archive - responds to .include?(slug) → bool
    module Checker
      def self.check_collection(col, archive)
        col.mods.filter_map do |slug|
          "#{col.name}: not in archive: #{slug}" unless archive.include?(slug)
        end
      end

      # collections - Hash<name, Collection>
      def self.check_modset(modset, collections, archive)
        errors = []

        modset.collections.each do |name|
          unless collections.key?(name)
            errors << "collection not found: #{name}"
            next
          end
          errors.concat(check_collection(collections[name], archive))
        end

        modset.mods.each do |slug|
          errors << "not in archive: #{slug}" unless archive.include?(slug)
        end

        errors
      end

      # collections - Array<Collection>
      # modsets     - Array<Modset>
      def self.check_all(collections, modsets, archive)
        errors = []

        collections.each do |col|
          errors.concat(check_collection(col, archive))
        end

        col_map = collections.each_with_object({}) { |c, h| h[c.name] = c }
        modsets.each do |ms|
          errors.concat(check_modset(ms, col_map, archive))
        end

        errors
      end
    end
  end
end
