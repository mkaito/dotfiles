# frozen_string_literal: true

module ModManager
  module Adapters
    module Download
      module Translator
        def self.file_info(h)
          FileInfo.new(
            file_id:  h["file_id"],
            name:     h["name"],
            version:  h["version"].to_s,
            category: h["category_name"].to_s,
            size_kb:  h["size_kb"].to_i,
          )
        end
      end
    end
  end
end
