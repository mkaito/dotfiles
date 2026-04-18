# frozen_string_literal: true

require "minitest/autorun"
require "stringio"
require "core/format"

class CoreFormatTest < Minitest::Test
  def setup
    @orig_no_color = ENV["NO_COLOR"]
    ENV.delete("NO_COLOR")
  end

  def teardown
    if @orig_no_color.nil?
      ENV.delete("NO_COLOR")
    else
      ENV["NO_COLOR"] = @orig_no_color
    end
  end

  # Default test env: $stdout is not a tty — all helpers pass through unchanged
  def test_no_color_when_not_tty
    assert_equal "hello", Core::Format.red("hello")
    assert_equal "hello", Core::Format.green("hello")
    assert_equal "hello", Core::Format.yellow("hello")
    assert_equal "hello", Core::Format.bold("hello")
    assert_equal "hello", Core::Format.dim("hello")
  end

  def test_no_color_env_disables_color
    ENV["NO_COLOR"] = "1"
    with_tty_stdout do
      assert_equal "x", Core::Format.red("x")
      assert_equal "x", Core::Format.green("x")
    end
  end

  def test_red_wraps_with_ansi_when_tty
    with_tty_stdout { assert_equal "\e[31mhello\e[0m", Core::Format.red("hello") }
  end

  def test_green_wraps_with_ansi_when_tty
    with_tty_stdout { assert_equal "\e[32mhello\e[0m", Core::Format.green("hello") }
  end

  def test_yellow_wraps_with_ansi_when_tty
    with_tty_stdout { assert_equal "\e[33mhello\e[0m", Core::Format.yellow("hello") }
  end

  def test_bold_wraps_with_ansi_when_tty
    with_tty_stdout { assert_equal "\e[1mhello\e[0m", Core::Format.bold("hello") }
  end

  def test_dim_wraps_with_ansi_when_tty
    with_tty_stdout { assert_equal "\e[2mhello\e[0m", Core::Format.dim("hello") }
  end

  def test_composable_nested_calls
    with_tty_stdout do
      result = Core::Format.bold(Core::Format.red("x"))
      assert_includes result, "\e[31mx\e[0m"
      assert result.start_with?("\e[1m")
    end
  end

  private

  def with_tty_stdout
    sio = StringIO.new
    sio.define_singleton_method(:tty?) { true }
    orig, $stdout = $stdout, sio
    yield
  ensure
    $stdout = orig
  end
end
