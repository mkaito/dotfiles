# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require_relative "../../lib/core/file_io"

class CoreFileIOTest < Minitest::Test
  def setup
    @dir = Dir.mktmpdir
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_atomic_write_creates_file
    path = File.join(@dir, "out.toml")
    Core::FileIO.atomic_write(path, "hello")
    assert_equal "hello", File.read(path)
  end

  def test_atomic_write_replaces_existing
    path = File.join(@dir, "out.toml")
    File.write(path, "old")
    Core::FileIO.atomic_write(path, "new")
    assert_equal "new", File.read(path)
  end

  def test_atomic_write_leaves_no_tmp_on_success
    path = File.join(@dir, "out.toml")
    Core::FileIO.atomic_write(path, "data")
    tmps = Dir.glob("#{@dir}/*.tmp.*")
    assert_empty tmps
  end
end
