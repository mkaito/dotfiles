# frozen_string_literal: true

require "minitest/autorun"
require "steam/vdf"

# Build a localconfig-shaped VDF with real tabs/newlines so byte-level
# assertions are meaningful. Covers: an app with LaunchOptions, an app without,
# an app whose value embeds escaped quotes, and a same-named key OUTSIDE the
# apps scope (must be ignored).
def sample_vdf
  t = ->(n) { "\t" * n }
  [
    %("UserLocalConfigStore"),
    %({),
    %(#{t[1]}"Software"),
    %(#{t[1]}{),
    %(#{t[2]}"Valve"),
    %(#{t[2]}{),
    %(#{t[3]}"Steam"),
    %(#{t[3]}{),
    %(#{t[4]}"apps"),
    %(#{t[4]}{),
    %(#{t[5]}"100"),
    %(#{t[5]}{),
    %(#{t[6]}"LaunchOptions"\t\t"mangohud %command%"),
    %(#{t[6]}"LastPlayed"\t\t"123"),
    %(#{t[5]}}),
    %(#{t[5]}"200"),
    %(#{t[5]}{),
    %(#{t[6]}"LastPlayed"\t\t"456"),
    %(#{t[5]}}),
    %(#{t[5]}"300"),
    %(#{t[5]}{),
    %(#{t[6]}"LaunchOptions"\t\t"bash -c \\"echo hi\\""),
    %(#{t[5]}}),
    %(#{t[4]}}),
    %(#{t[3]}}),
    %(#{t[2]}}),
    %(#{t[1]}}),
    %(#{t[1]}"OtherSection"),
    %(#{t[1]}{),
    %(#{t[2]}"LaunchOptions"\t\t"should-not-be-picked"),
    %(#{t[1]}}),
    %(}),
    ""
  ].join("\n")
end

class VdfLaunchOptionsTest < Minitest::Test
  def setup
    @src = sample_vdf
  end

  def test_extracts_launch_options_per_app
    lo = Steam::Vdf.launch_options(@src)
    assert_equal "mangohud %command%", lo["100"]
  end

  def test_app_without_launch_options_absent
    refute Steam::Vdf.launch_options(@src).key?("200")
  end

  def test_captures_escaped_value_opaquely
    # raw bytes between the delimiter quotes, backslashes preserved verbatim
    assert_equal 'bash -c \\"echo hi\\"', Steam::Vdf.launch_options(@src)["300"]
  end

  def test_ignores_same_named_key_outside_apps_scope
    refute_includes Steam::Vdf.launch_options(@src).values, "should-not-be-picked"
  end

  def test_app_ids_lists_all_blocks
    assert_equal %w[100 200 300], Steam::Vdf.app_ids(@src).sort
  end
end

class VdfApplyTest < Minitest::Test
  def setup
    @src = sample_vdf
  end

  def test_replace_changes_only_that_value
    result = Steam::Vdf.apply_launch_options(@src, {"100" => "newopts"})
    assert_equal 1, result[:applied]
    assert_empty result[:skipped]
    expected = @src.sub('"mangohud %command%"', '"newopts"')
    assert_equal expected, result[:src]
  end

  def test_insert_into_app_without_launch_options
    result = Steam::Vdf.apply_launch_options(@src, {"200" => "added"})
    assert_equal 1, result[:applied]
    # value now extractable and indentation matches siblings (6 tabs)
    assert_equal "added", Steam::Vdf.launch_options(result[:src])["200"]
    assert_includes result[:src], %(\t\t\t\t\t\t"LaunchOptions"\t\t"added")
    # the inserted line sits inside the 200 block, before LastPlayed
    block = result[:src][/"200"\n\t+\{(.+?)\t+\}/m, 1]
    assert_operator block.index("LaunchOptions"), :<, block.index("LastPlayed")
  end

  def test_appid_not_in_vdf_is_skipped_and_file_unchanged
    result = Steam::Vdf.apply_launch_options(@src, {"999" => "nope"})
    assert_equal 0, result[:applied]
    assert_equal ["999"], result[:skipped]
    assert_equal @src, result[:src]
  end

  def test_idempotent
    once = Steam::Vdf.apply_launch_options(@src, {"100" => "x", "200" => "y"})[:src]
    twice = Steam::Vdf.apply_launch_options(once, {"100" => "x", "200" => "y"})[:src]
    assert_equal once, twice
  end

  def test_opaque_round_trip_with_backslashes_and_quotes
    raw = 'weird \\"q\\" and stuff'
    out = Steam::Vdf.apply_launch_options(@src, {"300" => raw})[:src]
    assert_equal raw, Steam::Vdf.launch_options(out)["300"]
  end

  def test_multiple_apps_one_pass
    result = Steam::Vdf.apply_launch_options(@src, {"100" => "a", "200" => "b", "300" => "c"})
    assert_equal 3, result[:applied]
    lo = Steam::Vdf.launch_options(result[:src])
    assert_equal({"100" => "a", "200" => "b", "300" => "c"}, lo)
  end
end
