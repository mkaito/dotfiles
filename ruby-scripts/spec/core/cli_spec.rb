# frozen_string_literal: true

require "minitest/autorun"
require "stringio"
require_relative "../../lib/core/cli"

class CoreCLITest < Minitest::Test
  def test_confirm_returns_true_for_y
    assert Core::CLI.confirm("ok?", stdin: StringIO.new("y\n"), stdout: StringIO.new)
  end

  def test_confirm_returns_false_for_n
    refute Core::CLI.confirm("ok?", stdin: StringIO.new("n\n"), stdout: StringIO.new)
  end

  def test_confirm_returns_false_for_empty
    refute Core::CLI.confirm("ok?", stdin: StringIO.new("\n"), stdout: StringIO.new)
  end

  def test_confirm_prints_prompt
    out = StringIO.new
    Core::CLI.confirm("proceed?", stdin: StringIO.new("n\n"), stdout: out)
    assert_includes out.string, "proceed?"
  end
end
