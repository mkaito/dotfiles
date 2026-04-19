# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/adapters/deploy/deploy_tree"

include ModManager

class DeployTreeTest < Minitest::Test
  def solve(entries) = Adapters::Deploy::DeployTree.solve(entries)
  def e(dst_rel, mod_path_rel) = { dst_rel:, mod_path_rel: }

  def test_empty_returns_empty
    assert_equal [], solve([])
  end

  def test_single_mod_flat_file_produces_file_symlink
    result = solve([e("global.ini", "cp2077/cet/1.0")])
    assert_equal 1, result.size
    lnk = result.first
    assert_equal "cp2077/cet/1.0/global.ini", lnk[:src_rel]
    assert_equal "global.ini",                lnk[:dst_rel]
    assert_equal false,                        lnk[:dir]
  end

  def test_single_mod_subdir_produces_dir_symlink
    result = solve([e("bin/x64/global.ini", "cp2077/cet/1.0")])
    assert_equal 1, result.size
    lnk = result.first
    assert_equal "cp2077/cet/1.0/bin", lnk[:src_rel]
    assert_equal "bin",                lnk[:dst_rel]
    assert_equal true,                 lnk[:dir]
  end

  def test_single_mod_multiple_files_same_tree_produces_one_dir_symlink
    result = solve([
      e("bin/x64/plugins/cet/mods/nativeSettings/init.lua", "cp2077/native/1.0"),
      e("bin/x64/plugins/cet/mods/nativeSettings/other.lua", "cp2077/native/1.0"),
    ])
    assert_equal 1, result.size
    assert_equal "bin", result.first[:dst_rel]
    assert_equal true,  result.first[:dir]
  end

  def test_two_mods_separate_top_dirs_produce_two_dir_symlinks
    result = solve([
      e("r6/scripts/foo.reds",   "cp2077/foo/1.0"),
      e("archive/pc/mod/bar.xl", "cp2077/bar/2.0"),
    ])
    assert_equal 2, result.size
    assert result.all? { _1[:dir] }
    assert_equal %w[archive r6], result.map { _1[:dst_rel] }.sort
  end

  def test_two_mods_shared_parent_exclusive_children
    result = solve([
      e("bin/x64/plugins/cet/mods/nativeSettings/init.lua",  "cp2077/native/1.0"),
      e("bin/x64/plugins/cet/mods/vehicleHandling/init.lua", "cp2077/vhs/1.0"),
    ])
    dir_links  = result.select { _1[:dir] }
    file_links = result.reject { _1[:dir] }

    assert_empty file_links
    dst_rels = dir_links.map { _1[:dst_rel] }.sort
    assert_equal [
      "bin/x64/plugins/cet/mods/nativeSettings",
      "bin/x64/plugins/cet/mods/vehicleHandling",
    ], dst_rels

    native = dir_links.find { _1[:dst_rel].end_with?("nativeSettings") }
    assert_equal "cp2077/native/1.0/bin/x64/plugins/cet/mods/nativeSettings", native[:src_rel]

    vhs = dir_links.find { _1[:dst_rel].end_with?("vehicleHandling") }
    assert_equal "cp2077/vhs/1.0/bin/x64/plugins/cet/mods/vehicleHandling", vhs[:src_rel]
  end

  def test_two_exclusive_subdirs_under_shared_parent
    result = solve([
      e("r6/scripts/mod-a/init.reds",  "cp2077/mod-a/1.0"),
      e("r6/scripts/mod-b/extra.reds", "cp2077/mod-b/1.0"),
    ])
    assert_equal ["r6/scripts/mod-a", "r6/scripts/mod-b"], result.select { _1[:dir] }.map { _1[:dst_rel] }.sort
    assert_empty result.reject { _1[:dir] }
  end

  def test_loose_file_in_shared_dir_produces_file_symlink
    result = solve([
      e("r6/scripts/loose.reds",       "cp2077/mod-a/1.0"),
      e("r6/scripts/mod-b/extra.reds", "cp2077/mod-b/1.0"),
    ])
    assert_equal ["r6/scripts/mod-b"],    result.select { _1[:dir] }.map { _1[:dst_rel] }.sort
    assert_equal ["r6/scripts/loose.reds"], result.reject { _1[:dir] }.map { _1[:dst_rel] }.sort
  end

  def test_exclusive_subdir_deep_in_shared_tree
    result = solve([
      e("a/b/c/d/file.txt", "cp2077/mod-x/1.0"),
      e("a/b/e/file.txt",   "cp2077/mod-y/1.0"),
    ])
    assert_equal %w[a/b/c a/b/e], result.select { _1[:dir] }.map { _1[:dst_rel] }.sort
    assert_empty result.reject { _1[:dir] }
  end
end
