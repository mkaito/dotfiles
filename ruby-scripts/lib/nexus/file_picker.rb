# frozen_string_literal: true

module Nexus
  module FilePicker
    CATEGORY_ORDER = %w[MAIN OPTIONAL MISCELLANEOUS OLD_VERSION].freeze

    # Returns files sorted for display and index-based selection:
    #   - by category (MAIN first, then OPTIONAL, MISCELLANEOUS, OLD_VERSION, others)
    #   - within category: newest first by uploaded_timestamp, tie-break by file_id asc
    def self.sort_files(files)
      files.sort_by do |f|
        cat = CATEGORY_ORDER.index(f["category_name"]) || CATEGORY_ORDER.size
        [cat, -f["uploaded_timestamp"].to_i, f["file_id"].to_i]
      end
    end

    # Returns the single MAIN file if unambiguous; nil if multiple MAINs or none.
    def self.auto_select(sorted_files)
      mains = sorted_files.select { _1["category_name"] == "MAIN" }
      mains.first if mains.size == 1
    end

    # Prints a numbered, category-grouped file list to stdout.
    def self.print_file_list(sorted_files, stdout: $stdout)
      current_cat = nil
      sorted_files.each_with_index do |f, i|
        if f["category_name"] != current_cat
          stdout.puts "#{"\n" unless current_cat.nil?}#{f["category_name"]} FILES"
          current_cat = f["category_name"]
        end
        size_mb = (f["size_kb"].to_f / 1024).round(1)
        stdout.puts "  #{i + 1}. #{f["name"]} (#{f["version"]}, #{size_mb} MB)"
      end
      stdout.puts "\nRun again with --file N to download."
    end
  end
end
