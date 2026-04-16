# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require_relative "../../lib/mod_manager/verifier"
require_relative "../support/fake_config"

include ModManager

class VerifierTest < Minitest::Test
  def setup
    @dir      = Dir.mktmpdir
    @game_dir = File.join(@dir, "game")
    FileUtils.mkdir_p(@game_dir)
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_no_errors_when_all_present
    FileUtils.mkdir_p(File.join(@game_dir, "bin/x64"))
    File.write(File.join(@game_dir, "bin/x64/global.ini"), "")
    checks = [
      Verifier::Check.new(path: "bin/x64", type: "dir"),
      Verifier::Check.new(path: "bin/x64/global.ini", type: "file"),
    ]
    assert_empty Verifier.run(checks, @game_dir)
  end

  def test_missing_dir_reported
    checks = [Verifier::Check.new(path: "bin/x64", type: "dir")]
    errors = Verifier.run(checks, @game_dir)
    assert errors.any? { _1.include?("bin/x64") }
  end

  def test_missing_file_reported
    checks = [Verifier::Check.new(path: "bin/x64/global.ini", type: "file")]
    errors = Verifier.run(checks, @game_dir)
    assert errors.any? { _1.include?("bin/x64/global.ini") }
  end

  def test_collect_merges_config_modset_collections
    config = FakeConfig.new(
      archive_dir: @dir, collections_dir: @dir, modsets_dir: @dir,
      game_dir: @game_dir, domain: "cyberpunk2077",
      checks: [{ "path" => "bin/x64", "type" => "dir" }]
    )
    modset = Struct.new(:checks).new([{ "path" => "r6/scripts", "type" => "dir" }])
    col    = Struct.new(:checks).new([{ "path" => "bin/x64/global.ini", "type" => "file" }])

    checks = Verifier.collect(config, modset, [col])
    assert_equal 3, checks.size
    assert checks.any? { _1.path == "bin/x64" && _1.type == "dir" }
    assert checks.any? { _1.path == "r6/scripts" }
    assert checks.any? { _1.path == "bin/x64/global.ini" }
  end
end
