# frozen_string_literal: true

module Nexus
  module FilePicker
    CATEGORY_ORDER = %w[MAIN OPTIONAL MISCELLANEOUS OLD_VERSION].freeze

    # Returns raw file hashes sorted for display. Called inside Download::Nexus before
    # translation to FileInfo; needs uploaded_timestamp which is not part of FileInfo.
    def self.sort_files(files)
      files.sort_by do |f|
        cat = CATEGORY_ORDER.index(f["category_name"]) || CATEGORY_ORDER.size
        [cat, -f["uploaded_timestamp"].to_i, f["file_id"].to_i]
      end
    end

    # Returns the single MAIN FileInfo if unambiguous; nil if multiple MAINs or none.
    def self.auto_select(sorted_files)
      mains = sorted_files.select { _1.category == "MAIN" }
      mains.first if mains.size == 1
    end

    # Prints a numbered, category-grouped file list to stdout.
    def self.print_file_list(sorted_files, stdout: $stdout)
      current_cat = nil
      sorted_files.each_with_index do |f, i|
        if f.category != current_cat
          stdout.puts "#{"\n" unless current_cat.nil?}#{f.category} FILES"
          current_cat = f.category
        end
        size_mb = (f.size_kb.to_f / 1024).round(1)
        stdout.puts "  #{i + 1}. #{f.name} (#{f.version}, #{size_mb} MB)"
      end
      stdout.puts "\nRun again with --file N to download."
    end
  end
end
