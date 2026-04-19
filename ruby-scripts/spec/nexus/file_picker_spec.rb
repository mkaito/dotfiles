# frozen_string_literal: true

require "minitest/autorun"
require "nexus/file_picker"
require "mod_manager/core/models"

include ModManager

# Raw hash — used for sort_files (pre-translation, adapter-internal)
def raw_file(id:, cat:, ts:, name: "File #{id}", version: "1.0", size_kb: 1024)
  {"file_id" => id, "category_name" => cat, "uploaded_timestamp" => ts,
   "name" => name, "version" => version, "size_kb" => size_kb}
end

# FileInfo — used for auto_select and print_file_list (post-translation, port contract)
def file_info(id:, cat:, name: "File #{id}", version: "1.0", size_kb: 1024)
  FileInfo.new(file_id: id, category: cat, name:, version:, size_kb:)
end

RAW_MAIN_A = raw_file(id: 1, cat: "MAIN", ts: 1000, name: "Main A").freeze
RAW_MAIN_B = raw_file(id: 2, cat: "MAIN", ts: 900, name: "Main B").freeze
RAW_OLD_C = raw_file(id: 3, cat: "OLD_VERSION", ts: 800, name: "Old C").freeze
RAW_OPT_D = raw_file(id: 4, cat: "OPTIONAL", ts: 700, name: "Optional D").freeze

FI_MAIN_A = file_info(id: 1, cat: "MAIN", name: "Main A").freeze
FI_MAIN_B = file_info(id: 2, cat: "MAIN", name: "Main B").freeze
FI_OLD_C = file_info(id: 3, cat: "OLD_VERSION", name: "Old C").freeze
FI_OPT_D = file_info(id: 4, cat: "OPTIONAL", name: "Optional D").freeze

class FilePickerSortFilesTest < Minitest::Test
  def test_main_before_optional_before_old
    sorted = Nexus::FilePicker.sort_files([RAW_OLD_C, RAW_OPT_D, RAW_MAIN_A])
    assert_equal %w[MAIN OPTIONAL OLD_VERSION], sorted.map { it["category_name"] }
  end

  def test_within_category_newest_first
    sorted = Nexus::FilePicker.sort_files([RAW_MAIN_B, RAW_MAIN_A])
    assert_equal [1, 2], sorted.map { it["file_id"] }
  end

  def test_tie_break_by_file_id_asc
    a = raw_file(id: 10, cat: "MAIN", ts: 500)
    b = raw_file(id: 5, cat: "MAIN", ts: 500)
    sorted = Nexus::FilePicker.sort_files([a, b])
    assert_equal [5, 10], sorted.map { it["file_id"] }
  end

  def test_unknown_category_last
    unknown = raw_file(id: 99, cat: "FOOBAZ", ts: 9999)
    sorted = Nexus::FilePicker.sort_files([unknown, RAW_MAIN_A])
    assert_equal "MAIN", sorted.first["category_name"]
  end

  def test_deterministic_with_same_input
    files = [RAW_OLD_C, RAW_MAIN_B, RAW_OPT_D, RAW_MAIN_A]
    assert_equal Nexus::FilePicker.sort_files(files), Nexus::FilePicker.sort_files(files)
  end
end

class FilePickerAutoSelectTest < Minitest::Test
  def test_single_main_auto_selected
    assert_equal 1, Nexus::FilePicker.auto_select([FI_MAIN_A]).file_id
  end

  def test_single_main_selected_even_with_other_categories_present
    assert_equal 1, Nexus::FilePicker.auto_select([FI_MAIN_A, FI_OLD_C]).file_id
  end

  def test_multiple_main_returns_nil
    assert_nil Nexus::FilePicker.auto_select([FI_MAIN_A, FI_MAIN_B])
  end

  def test_no_main_returns_nil
    assert_nil Nexus::FilePicker.auto_select([FI_OLD_C, FI_OPT_D])
  end

  def test_empty_returns_nil
    assert_nil Nexus::FilePicker.auto_select([])
  end
end

class FilePickerPrintFileListTest < Minitest::Test
  def test_groups_by_category_with_headers
    out = StringIO.new
    Nexus::FilePicker.print_file_list([FI_MAIN_A, FI_OLD_C], stdout: out)
    assert_match(/MAIN FILES/, out.string)
    assert_match(/OLD_VERSION FILES/, out.string)
  end

  def test_shows_1_based_index
    out = StringIO.new
    Nexus::FilePicker.print_file_list([FI_MAIN_A, FI_MAIN_B], stdout: out)
    assert_match(/^\s+1\./, out.string)
    assert_match(/^\s+2\./, out.string)
  end

  def test_shows_name_version_size
    out = StringIO.new
    Nexus::FilePicker.print_file_list([FI_MAIN_A], stdout: out)
    assert_match(/Main A/, out.string)
    assert_match(/1\.0/, out.string)
    assert_match(/1\.0 MB/, out.string)   # 1024 KB = 1.0 MB
  end

  def test_shows_rerun_hint
    out = StringIO.new
    Nexus::FilePicker.print_file_list([FI_MAIN_A], stdout: out)
    assert_match(/--file/, out.string)
  end
end
