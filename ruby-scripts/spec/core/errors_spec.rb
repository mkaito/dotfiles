# frozen_string_literal: true

require "minitest/autorun"
require "core/errors"

class CoreErrorTest < Minitest::Test
  def test_error_is_standard_error
    assert_kind_of StandardError, Core::Error.new("msg")
  end

  def test_error_message
    assert_equal "bad thing", Core::Error.new("bad thing").message
  end
end

class CoreValidationErrorTest < Minitest::Test
  def test_is_core_error
    assert_kind_of Core::Error, Core::ValidationError.new(["x"])
  end

  def test_errors_attribute
    e = Core::ValidationError.new(["missing name", "missing slug"])
    assert_equal ["missing name", "missing slug"], e.errors
  end

  def test_message_joins_errors
    e = Core::ValidationError.new(["missing name", "missing slug"])
    assert_equal "missing name; missing slug", e.message
  end

  def test_single_error
    e = Core::ValidationError.new(["missing name"])
    assert_equal "missing name", e.message
    assert_equal 1, e.errors.size
  end
end
