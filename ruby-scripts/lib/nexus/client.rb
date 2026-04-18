# frozen_string_literal: true

require "net/http"
require "uri"
require "json"
require "time"
require "fileutils"
require "core/errors"
require "core/log"
require "core/xdg"

module Nexus
  class Client
    BASE    = "https://api.nexusmods.com/v1"
    BASE_V2 = "https://api.nexusmods.com/v2/graphql"

    # Rate limits (as of 2026): 500 req/hour, 20,000 req/day.
    # Hourly limit resets on the clock hour UTC; daily at 00:00 UTC.
    # Headers on every response: X-RL-Hourly-Limit, X-RL-Hourly-Remaining, X-RL-Hourly-Reset,
    #                             X-RL-Daily-Limit,  X-RL-Daily-Remaining,  X-RL-Daily-Reset.
    # X-RL-Hourly-Reset format: "2019-02-01T12:00:00+00:00"
    # X-RL-Daily-Reset  format: "2019-02-02 00:00:00 +0000"
    # 429 is returned when the limit is exceeded; sleep until the reset timestamp, then retry.
    HOURLY_WARN_THRESHOLD = 50
    DAILY_WARN_THRESHOLD  = 200

    # TTL for cached JSON API responses. Mod metadata is immutable once published;
    # collection revisions are immutable. Cache aggressively.
    MOD_INFO_TTL       = 86_400      # 24 h — mod metadata rarely changes
    MOD_FILES_TTL      = 86_400      # 24 h — file list stable within the day
    REVISION_TTL       = Float::INFINITY  # specific revision is immutable

    # GraphQL query: fetch one revision (specific or latest) + its mod list.
    COLLECTION_REVISION_QUERY = <<~GQL.freeze
      query GetCollectionRevision($domainName: String!, $slug: String!, $revision: Int!) {
        collectionRevision(domainName: $domainName, slug: $slug, revision: $revision) {
          collection { name slug }
          revisionNumber
          downloadLink
          modFiles { fileId file { modId name version } }
        }
      }
    GQL

    COLLECTION_LATEST_QUERY = <<~GQL.freeze
      query GetCollection($domainName: String!, $slug: String!) {
        collection(domainName: $domainName, slug: $slug) {
          name
          slug
          latestPublishedRevision {
            revisionNumber
            downloadLink
            modFiles { fileId file { modId name version } }
          }
        }
      }
    GQL

    COLLECTION_REVISIONS_QUERY = <<~GQL.freeze
      query ListRevisions($domainName: String!, $slug: String!) {
        collection(domainName: $domainName, slug: $slug) {
          name
          revisions {
            revisionNumber
            createdAt
            revisionStatus
            modCount
          }
        }
      }
    GQL

    def initialize(api_key)
      @api_key        = api_key
      @warned_hourly  = false
      @warned_daily   = false
    end

    def mod_info(game_domain, mod_id)
      cached_json(api_cache("#{game_domain}/mods/#{mod_id}/info.json"), ttl: MOD_INFO_TTL) do
        get("/games/#{game_domain}/mods/#{mod_id}")
      end
    end

    def mod_files(game_domain, mod_id)
      cached_json(api_cache("#{game_domain}/mods/#{mod_id}/files.json"), ttl: MOD_FILES_TTL) do
        get("/games/#{game_domain}/mods/#{mod_id}/files")
      end.then { _1["files"] }
    end

    def download_urls(game_domain, mod_id, file_id)
      # CDN URLs expire — never cache.
      get("/games/#{game_domain}/mods/#{mod_id}/files/#{file_id}/download_link")
    end

    # Returns parsed GraphQL data for one revision. Pass revision: nil for latest.
    def collection_revision(game_domain, slug, revision: nil)
      if revision
        cached_json(api_cache("#{game_domain}/collections/#{slug}/revision-#{revision}.json"),
                    ttl: REVISION_TTL) do
          graphql(COLLECTION_REVISION_QUERY, { domainName: game_domain, slug:, revision: })
        end
      else
        # Latest revision: not cached (changes when author publishes).
        graphql(COLLECTION_LATEST_QUERY, { domainName: game_domain, slug: })
      end
    end

    # Returns parsed GraphQL data listing all revisions of a collection.
    def collection_revisions(game_domain, slug)
      graphql(COLLECTION_REVISIONS_QUERY, { domainName: game_domain, slug: })
    end

    # GETs the relative download_link path returned by GraphQL and returns the CDN URL string.
    def collection_download_url(path)
      uri = URI("https://api.nexusmods.com#{path}")
      req = Net::HTTP::Get.new(uri)
      req["apikey"]              = @api_key
      req["Application-Name"]    = "ruby-mod-manager"
      req["Application-Version"] = "1.0.0"
      res = request_with_retry { Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |h| h.request(req) } }
      check_rate_limits(res)
      raise Core::Error, "Nexus collection download link #{res.code}: #{res.body}" unless res.is_a?(Net::HTTPSuccess)
      body  = JSON.parse(res.body)
      links = body["download_links"] or raise Core::Error, "no download_links in response"
      preferred = links.find { _1["short_name"] == "Nexus CDN" }
      (preferred || links.first)["URI"]
    end

    private

    def graphql(query, variables = {})
      uri = URI(BASE_V2)
      req = Net::HTTP::Post.new(uri)
      req["apikey"]              = @api_key
      req["Content-Type"]        = "application/json"
      req["Application-Name"]    = "ruby-mod-manager"
      req["Application-Version"] = "1.0.0"
      req.body = JSON.generate({ query:, variables: })
      res = request_with_retry { Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |h| h.request(req) } }
      check_rate_limits(res)
      raise Core::Error, "Nexus GraphQL #{res.code}: #{res.body}" unless res.is_a?(Net::HTTPSuccess)
      body = JSON.parse(res.body)
      raise Core::Error, "Nexus GraphQL errors: #{body["errors"]}" if body["errors"]
      body["data"]
    end

    def get(path)
      uri = URI("#{BASE}#{path}")
      req = Net::HTTP::Get.new(uri)
      req["apikey"]              = @api_key
      req["Application-Name"]    = "ruby-mod-manager"
      req["Application-Version"] = "1.0.0"
      res = request_with_retry { Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |h| h.request(req) } }
      check_rate_limits(res)
      raise Core::Error, "Nexus API #{res.code}: #{res.body}" unless res.is_a?(Net::HTTPSuccess)
      JSON.parse(res.body)
    end

    # On 429: tell the user when the limit resets and abort.
    def request_with_retry
      res = yield
      return res unless res.code.to_i == 429

      reset_at = parse_reset_time(res["x-rl-hourly-reset"] || res["x-rl-daily-reset"])
      msg = "Nexus rate limit exceeded"
      msg += " — resets at #{reset_at.utc.strftime("%H:%M UTC")}" if reset_at
      raise Core::Error, msg
    end

    # Warn once per session when dropping below thresholds.
    def check_rate_limits(res)
      h       = res["x-rl-hourly-remaining"]&.to_i
      h_reset = res["x-rl-hourly-reset"]
      d       = res["x-rl-daily-remaining"]&.to_i
      d_reset = res["x-rl-daily-reset"]

      if h && h < HOURLY_WARN_THRESHOLD && !@warned_hourly
        reset_str = h_reset ? " (resets #{parse_reset_time(h_reset)&.utc&.strftime("%H:%M UTC")})" : ""
        Core::Log.warn("#{h}/#{res["x-rl-hourly-limit"]} Nexus hourly requests remaining#{reset_str}")
        @warned_hourly = true
      end

      if d && d < DAILY_WARN_THRESHOLD && !@warned_daily
        reset_str = d_reset ? " (resets #{parse_reset_time(d_reset)&.utc&.strftime("%H:%M UTC")})" : ""
        Core::Log.warn("#{d}/#{res["x-rl-daily-limit"]} Nexus daily requests remaining#{reset_str}")
        @warned_daily = true
      end
    end

    def parse_reset_time(value)
      return nil unless value
      Time.parse(value)
    rescue ArgumentError
      nil
    end

    # Cache parsed JSON from block to path. Returns cached data if file is fresher than ttl seconds.
    def cached_json(path, ttl:)
      if File.exist?(path)
        age = Time.now - File.mtime(path)
        return JSON.parse(File.read(path)) if ttl == Float::INFINITY || age < ttl
      end
      data = yield
      FileUtils.mkdir_p(File.dirname(path))
      File.write(path, JSON.generate(data))
      data
    end

    def api_cache(*parts)
      File.join(Core::XDG.cache_home, "mods", "nexus", "api", *parts)
    end
  end
end
