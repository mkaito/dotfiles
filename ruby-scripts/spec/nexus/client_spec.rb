# frozen_string_literal: true

require "minitest/autorun"
require "net/http"
require_relative "../../lib/nexus/client"

class NexusClientCheckRateLimitsTest < Minitest::Test
  def setup
    @client = Nexus::Client.new("test-key")
  end

  def test_warns_once_when_hourly_low
    res = fake_response("x-rl-hourly-remaining" => "5", "x-rl-hourly-limit" => "500",
                        "x-rl-daily-remaining"  => "1000")
    out1 = capture_io { @client.send(:check_rate_limits, res) }.first
    out2 = capture_io { @client.send(:check_rate_limits, res) }.first
    assert_match(/hourly/, out1)
    assert_empty out2, "should not warn twice in the same session"
  end

  def test_warns_once_when_daily_low
    res = fake_response("x-rl-hourly-remaining" => "200", "x-rl-daily-remaining" => "10",
                        "x-rl-daily-limit" => "20000")
    out1 = capture_io { @client.send(:check_rate_limits, res) }.first
    out2 = capture_io { @client.send(:check_rate_limits, res) }.first
    assert_match(/daily/, out1)
    assert_empty out2
  end

  def test_no_warn_when_limits_healthy
    res = fake_response("x-rl-hourly-remaining" => "400", "x-rl-daily-remaining" => "10000")
    out = capture_io { @client.send(:check_rate_limits, res) }.first
    assert_empty out
  end

  def test_no_warn_when_headers_absent
    res = fake_response({})
    out = capture_io { @client.send(:check_rate_limits, res) }.first
    assert_empty out
  end

  def test_includes_reset_time_in_warning
    reset = (Time.now + 1800).utc.strftime("%Y-%m-%dT%H:%M:%S+00:00")
    res = fake_response("x-rl-hourly-remaining" => "1", "x-rl-hourly-limit" => "500",
                        "x-rl-hourly-reset" => reset, "x-rl-daily-remaining" => "5000")
    out = capture_io { @client.send(:check_rate_limits, res) }.first
    assert_match(/resets.*UTC/, out)
  end

  private

  def fake_response(headers)
    Object.new.tap { |r| r.define_singleton_method(:[]) { |k| headers[k] } }
  end
end
