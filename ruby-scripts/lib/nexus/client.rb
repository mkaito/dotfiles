# frozen_string_literal: true

require "net/http"
require "uri"
require "json"
require_relative "../core/errors"
require_relative "../core/log"
require_relative "../core/format"

module Nexus
  class Client
    BASE    = "https://api.nexusmods.com/v1"
    BASE_V2 = "https://api.nexusmods.com/v2/graphql"

    # GraphQL query: fetch one revision (specific or latest) + its mod list.
    # Field names verified against Nexus GraphQL schema; adjust if API changes.
    COLLECTION_REVISION_QUERY = <<~GQL.freeze
      query GetCollectionRevision($domainName: String!, $slug: String!, $revision: Int!) {
        collectionRevision(domainName: $domainName, slug: $slug, revision: $revision) {
          collection { name slug }
          revisionNumber
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

    # Returns parsed GraphQL data for one revision. Pass revision: nil for latest.
    def collection_revision(game_domain, slug, revision: nil)
      if revision
        graphql(COLLECTION_REVISION_QUERY, { domainName: game_domain, slug: slug, revision: revision })
      else
        graphql(COLLECTION_LATEST_QUERY, { domainName: game_domain, slug: slug })
      end
    end

    # Returns parsed GraphQL data listing all revisions of a collection.
    def collection_revisions(game_domain, slug)
      graphql(COLLECTION_REVISIONS_QUERY, { domainName: game_domain, slug: slug })
    end

    private

    def graphql(query, variables = {})
      uri = URI(BASE_V2)
      req = Net::HTTP::Post.new(uri)
      req["apikey"]              = @api_key
      req["Content-Type"]        = "application/json"
      req["Application-Name"]    = "ruby-mod-manager"
      req["Application-Version"] = "1.0.0"
      req.body = JSON.generate({ query: query, variables: variables })
      res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |h| h.request(req) }
      warn_rate_limits(res)
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
      res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |h| h.request(req) }
      warn_rate_limits(res)
      raise Core::Error, "Nexus API #{res.code}: #{res.body}" unless res.is_a?(Net::HTTPSuccess)
      JSON.parse(res.body)
    end

    def warn_rate_limits(res)
      h = res["x-rl-hourly-remaining"]&.to_i
      d = res["x-rl-daily-remaining"]&.to_i
      puts Core::Format.yellow("warning: Nexus hourly API limit low (#{h} remaining)") if h && h < 10
      puts Core::Format.yellow("warning: Nexus daily API limit low (#{d} remaining)")  if d && d < 50
    end
  end
end
