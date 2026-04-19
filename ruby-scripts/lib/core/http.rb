# frozen_string_literal: true

require "net/http"
require "uri"
require "fileutils"
require "core/errors"
require "core/log"

module Core
  module Http
    # Downloads url to cache_path if not already present. Returns cache_path.
    # The block is called only on a cache miss; it must return the URL to fetch.
    def self.cached_download(cache_path, label: nil, &url_block)
      return cache_path if File.exist?(cache_path)
      Core::Log.debug("cache miss: #{File.basename(cache_path)}")
      puts "downloading #{label || File.basename(cache_path)}..."
      FileUtils.mkdir_p(File.dirname(cache_path))
      download(url_block.call, cache_path)
      cache_path
    end

    def self.download(url, dest)
      uri = URI(url.gsub(" ", "%20"))
      Net::HTTP.start(uri.hostname, uri.port, use_ssl: uri.scheme == "https") do |http|
        http.request(Net::HTTP::Get.new(uri)) do |res|
          raise Core::Error, "download failed: HTTP #{res.code}" unless res.is_a?(Net::HTTPSuccess)
          File.open(dest, "wb") { |f| res.read_body { f.write(it) } }
        end
      end
    end
  end
end
