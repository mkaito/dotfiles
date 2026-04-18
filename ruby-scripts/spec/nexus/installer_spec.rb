# frozen_string_literal: true

require "minitest/autorun"
require_relative "../../lib/nexus/installer"

# Helpers
def file(id:, cat:, ts:, name: "File #{id}", version: "1.0", size_kb: 1024)
  { "file_id" => id, "category_name" => cat, "uploaded_timestamp" => ts,
    "name" => name, "version" => version, "size_kb" => size_kb, "file_name" => "file#{id}.zip" }
end

MAIN_A  = file(id: 1, cat: "MAIN",      ts: 1000, name: "Main A").freeze
MAIN_B  = file(id: 2, cat: "MAIN",      ts: 900,  name: "Main B").freeze
OLD_C   = file(id: 3, cat: "OLD_VERSION", ts: 800, name: "Old C").freeze
OPT_D   = file(id: 4, cat: "OPTIONAL",  ts: 700,  name: "Optional D").freeze

class InstallerSortFilesTest < Minitest::Test
  def test_main_before_optional_before_old
    sorted = Nexus::Installer.sort_files([OLD_C, OPT_D, MAIN_A])
    assert_equal %w[MAIN OPTIONAL OLD_VERSION], sorted.map { _1["category_name"] }
  end

  def test_within_category_newest_first
    sorted = Nexus::Installer.sort_files([MAIN_B, MAIN_A])
    assert_equal [1, 2], sorted.map { _1["file_id"] }
  end

  def test_tie_break_by_file_id_asc
    a = file(id: 10, cat: "MAIN", ts: 500)
    b = file(id:  5, cat: "MAIN", ts: 500)
    sorted = Nexus::Installer.sort_files([a, b])
    assert_equal [5, 10], sorted.map { _1["file_id"] }
  end

  def test_unknown_category_last
    unknown = file(id: 99, cat: "FOOBAZ", ts: 9999)
    sorted = Nexus::Installer.sort_files([unknown, MAIN_A])
    assert_equal "MAIN", sorted.first["category_name"]
  end

  def test_deterministic_with_same_input
    files = [OLD_C, MAIN_B, OPT_D, MAIN_A]
    assert_equal Nexus::Installer.sort_files(files), Nexus::Installer.sort_files(files)
  end
end

class InstallerAutoSelectTest < Minitest::Test
  def test_single_main_auto_selected
    assert_equal 1, Nexus::Installer.auto_select([MAIN_A])["file_id"]
  end

  def test_single_main_selected_even_with_other_categories_present
    assert_equal 1, Nexus::Installer.auto_select([MAIN_A, OLD_C])["file_id"]
  end

  def test_multiple_main_returns_nil
    assert_nil Nexus::Installer.auto_select([MAIN_A, MAIN_B])
  end

  def test_no_main_returns_nil
    assert_nil Nexus::Installer.auto_select([OLD_C, OPT_D])
  end

  def test_empty_returns_nil
    assert_nil Nexus::Installer.auto_select([])
  end
end

class InstallerPrintFileListTest < Minitest::Test
  def test_groups_by_category_with_headers
    out = StringIO.new
    Nexus::Installer.print_file_list([MAIN_A, OLD_C], stdout: out)
    assert_match(/MAIN FILES/, out.string)
    assert_match(/OLD_VERSION FILES/, out.string)
  end

  def test_shows_1_based_index
    out = StringIO.new
    Nexus::Installer.print_file_list([MAIN_A, MAIN_B], stdout: out)
    assert_match(/^\s+1\./, out.string)
    assert_match(/^\s+2\./, out.string)
  end

  def test_shows_name_version_size
    out = StringIO.new
    Nexus::Installer.print_file_list([MAIN_A], stdout: out)
    assert_match(/Main A/, out.string)
    assert_match(/1\.0/, out.string)
    assert_match(/1\.0 MB/, out.string)   # 1024 KB = 1.0 MB
  end

  def test_shows_rerun_hint
    out = StringIO.new
    Nexus::Installer.print_file_list([MAIN_A], stdout: out)
    assert_match(/--file/, out.string)
  end
end

class InstallerFileSlugTest < Minitest::Test
  # slug derives from file name with version suffix stripped
  def file_slug(name, version)
    base = name.sub(/\s+#{Regexp.escape(version)}\z/, "")
    Nexus::Installer.send(:slugify, base)
  end

  def test_strips_version_from_file_name
    assert_equal "nexus-9223-42-cet-no-forced-weapons-all-in-one-1.1.3",
                 "nexus-9223-42-#{file_slug("CET. No forced weapons all-in-one 1.1.3", "1.1.3")}-1.1.3"
  end

  def test_strips_version_from_redscript_variant
    assert_equal "reds-b-default-initial-weapon-equip",
                 file_slug("Reds.B. Default initial weapon equip 1.1.0", "1.1.0")
  end

  def test_no_version_in_name_unchanged
    assert_equal "cyber-engine-tweaks",
                 file_slug("Cyber Engine Tweaks", "1.5.0")
  end
end

class InstallerSlugifyTest < Minitest::Test
  def slugify(name) = Nexus::Installer.send(:slugify, name)

  def test_spaces_become_hyphens
    assert_equal "cyber-engine-tweaks", slugify("Cyber Engine Tweaks")
  end

  def test_special_chars_stripped
    assert_equal "mod-name", slugify('Mod!@#$Name')
  end

  def test_leading_trailing_hyphens_removed
    assert_equal "mod", slugify("--mod--")
  end

  def test_consecutive_separators_collapsed
    assert_equal "a-b", slugify("A   B")
  end

  def test_numbers_preserved
    assert_equal "cet-1-5-0", slugify("CET 1.5.0")
  end
end

class InstallerDetectRootTest < Minitest::Test
  def test_single_subdir_stripped
    Dir.mktmpdir do |tmp|
      inner = File.join(tmp, "ModFolder")
      Dir.mkdir(inner)
      assert_equal inner, Nexus::Installer.send(:detect_root, tmp)
    end
  end

  def test_multiple_entries_not_stripped
    Dir.mktmpdir do |tmp|
      Dir.mkdir(File.join(tmp, "A"))
      Dir.mkdir(File.join(tmp, "B"))
      assert_equal tmp, Nexus::Installer.send(:detect_root, tmp)
    end
  end

  def test_single_file_not_stripped
    Dir.mktmpdir do |tmp|
      File.write(File.join(tmp, "readme.txt"), "")
      assert_equal tmp, Nexus::Installer.send(:detect_root, tmp)
    end
  end
end

class InstallerPickUrlTest < Minitest::Test
  def test_prefers_nexus_cdn
    urls = [
      { "short_name" => "Mirror", "URI" => "https://mirror.example.com/file" },
      { "short_name" => "Nexus CDN", "URI" => "https://nexus.example.com/file" }
    ]
    assert_equal "https://nexus.example.com/file", Nexus::Installer.send(:pick_url, urls)
  end

  def test_falls_back_to_first
    urls = [{ "short_name" => "Mirror", "URI" => "https://mirror.example.com/file" }]
    assert_equal "https://mirror.example.com/file", Nexus::Installer.send(:pick_url, urls)
  end

  def test_raises_on_empty
    assert_raises(Core::Error) { Nexus::Installer.send(:pick_url, []) }
  end
end

class InstallerCacheTest < Minitest::Test
  def test_cache_hit_skips_url_block
    Dir.mktmpdir do |cache_dir|
      stub_xdg(cache_dir) do
        f = file(id: 42, cat: "MAIN", ts: 1000, name: "test mod")
        game_domain, mod_id = "cyberpunk2077", 999
        cache_path = File.join(cache_dir, "mods", "nexus", game_domain, mod_id.to_s,
                               "42", f["file_name"])
        FileUtils.mkdir_p(File.dirname(cache_path))
        File.write(cache_path, "fake archive")

        url_called = false
        result = Nexus::Installer.send(:cached_archive, game_domain, mod_id, f) do
          url_called = true
          "http://example.com/file"
        end

        refute url_called, "url block should not be called on cache hit"
        assert_equal cache_path, result
      end
    end
  end

  def test_cache_miss_calls_url_block
    Dir.mktmpdir do |cache_dir|
      stub_xdg(cache_dir) do
        f = file(id: 43, cat: "MAIN", ts: 1000)
        url_called = false
        # Block is called; download then fails (no server) — that's expected
        assert_raises(StandardError) do
          Nexus::Installer.send(:cached_archive, "cyberpunk2077", 888, f) do
            url_called = true
            "http://127.0.0.1:1/file.zip"
          end
        end
        assert url_called, "url block must be called on cache miss"
      end
    end
  end

  private

  def stub_xdg(dir)
    orig = ENV["XDG_CACHE_HOME"]
    ENV["XDG_CACHE_HOME"] = dir
    yield
  ensure
    orig.nil? ? ENV.delete("XDG_CACHE_HOME") : ENV["XDG_CACHE_HOME"] = orig
  end
end
