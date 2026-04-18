# frozen_string_literal: true

require "minitest/autorun"
require "net/http"
require_relative "../../lib/nexus/client"

class NexusClientWarnRateLimitsTest < Minitest::Test
  def setup
    @client = Nexus::Client.new("test-key")
  end

  def test_warns_when_hourly_low
    res = fake_response({ "x-rl-hourly-remaining" => "5", "x-rl-daily-remaining" => "100" })
    out = capture_io { @client.send(:warn_rate_limits, res) }.first
    assert_match(/hourly.*limit low/, out)
    refute_match(/daily.*limit low/, out)
  end

  def test_warns_when_daily_low
    res = fake_response({ "x-rl-hourly-remaining" => "50", "x-rl-daily-remaining" => "20" })
    out = capture_io { @client.send(:warn_rate_limits, res) }.first
    refute_match(/hourly.*limit low/, out)
    assert_match(/daily.*limit low/, out)
  end

  def test_no_warn_when_limits_healthy
    res = fake_response({ "x-rl-hourly-remaining" => "200", "x-rl-daily-remaining" => "500" })
    out = capture_io { @client.send(:warn_rate_limits, res) }.first
    assert_empty out
  end

  def test_no_warn_when_headers_absent
    res = fake_response({})
    out = capture_io { @client.send(:warn_rate_limits, res) }.first
    assert_empty out
  end

  def test_boundary_at_ten_no_warn
    res = fake_response({ "x-rl-hourly-remaining" => "10", "x-rl-daily-remaining" => "50" })
    out = capture_io { @client.send(:warn_rate_limits, res) }.first
    assert_empty out
  end

  private

  def fake_response(headers)
    h = headers
    Object.new.tap { |r| r.define_singleton_method(:[]) { |k| h[k] } }
  end
end
