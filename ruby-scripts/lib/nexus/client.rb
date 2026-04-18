# frozen_string_literal: true

require "net/http"
require "uri"
require "json"
require_relative "../core/errors"
require_relative "../core/log"

module Nexus
  class Client
    BASE = "https://api.nexusmods.com/v1"

    def initialize(api_key)
      @api_key = api_key
    end

    def mod_info(game_domain, mod_id)
      get("/games/#{game_domain}/mods/#{mod_id}")
    end

    def mod_files(game_domain, mod_id)
      get("/games/#{game_domain}/mods/#{mod_id}/files")["files"]
    end

    def download_urls(game_domain, mod_id, file_id)
      get("/games/#{game_domain}/mods/#{mod_id}/files/#{file_id}/download_link")
    end

    private

    def get(path)
      uri = URI("#{BASE}#{path}")
      req = Net::HTTP::Get.new(uri)
      req["apikey"]              = @api_key
      req["Application-Name"]    = "ruby-mod-manager"
      req["Application-Version"] = "1.0.0"
      res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |h| h.request(req) }
      warn_rate_limits(res)
      raise Core::Error, "Nexus API #{res.code}: #{res.body}" unless res.is_a?(Net::HTTPSuccess)
      JSON.parse(res.body)
    end

    def warn_rate_limits(res)
      h = res["x-rl-hourly-remaining"]&.to_i
      d = res["x-rl-daily-remaining"]&.to_i
      Core::Log.warn("Nexus hourly limit low: #{h} remaining") if h && h < 10
      Core::Log.warn("Nexus daily limit low: #{d} remaining")  if d && d < 50
    end
  end
end
