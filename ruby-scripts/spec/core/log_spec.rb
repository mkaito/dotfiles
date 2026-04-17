# frozen_string_literal: true

require "minitest/autorun"
require_relative "../../lib/core/log"

class CoreLogTest < Minitest::Test
  def test_info_appears_at_default_level
    out, = with_log_level("info") { Core::Log.info("hello") }
    assert_includes out, "hello"
  end

  def test_debug_suppressed_at_info_level
    out, = with_log_level("info") { Core::Log.debug("secret") }
    assert_empty out
  end

  def test_debug_appears_at_debug_level
    out, = with_log_level("debug") { Core::Log.debug("verbose") }
    assert_includes out, "verbose"
  end

  def test_warn_suppressed_at_error_level
    out, = with_log_level("error") { Core::Log.warn("noise") }
    assert_empty out
  end

  def test_output_goes_to_stdout
    out, err = with_log_level("info") { Core::Log.warn("check") }
    assert_includes out, "check"
    assert_empty err
  end

  def test_label_format
    out, = with_log_level("info") { Core::Log.warn("msg") }
    assert_match(/\[WARN\] msg/, out)
  end

  private

  def with_log_level(level, &)
    prev = ENV["RUBY_LOG_LEVEL"]
    ENV["RUBY_LOG_LEVEL"] = level
    capture_io(&)
  ensure
    ENV["RUBY_LOG_LEVEL"] = prev
  end
end
