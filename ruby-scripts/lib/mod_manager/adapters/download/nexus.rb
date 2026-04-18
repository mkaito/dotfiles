# frozen_string_literal: true

require "tmpdir"
require "fileutils"
require_relative "../../errors"

module ModManager
  module Adapters
    module Download
      class Nexus
        def initialize(game_domain, client)
          @game_domain = game_domain
          @client      = client
        end

        # Returns Array<Hash> of file hashes from Nexus API.
        # Keys: file_id, name, version, category_name, size_kb, uploaded_timestamp, file_name.
        def list_files(mod_id:)
          ::Nexus::Installer.sort_files(@client.mod_files(@game_domain, mod_id.to_i))
        end

        # Downloads + unpacks to a temp dir. Caller must clean up unpacked.tmp_dir.
        # Returns UnpackedMod.
        def fetch(mod_id:, file_id:, slug: nil)
          info  = @client.mod_info(@game_domain, mod_id.to_i)
          files = @client.mod_files(@game_domain, mod_id.to_i)
          file  = files.find { _1["file_id"] == file_id } or
                  raise Core::Error, "file_id #{file_id} not found for mod #{mod_id}"

          version   = file["version"].to_s.strip.then { _1.empty? ? info["version"].to_s.strip : _1 }
          raise Core::Error, "could not determine version for mod #{mod_id}" if version.empty?

          base_slug = slugify(file["name"].to_s.sub(/\s+#{Regexp.escape(version)}\z/, ""))
          slug    ||= "nexus-#{mod_id}-#{file["file_id"]}-#{base_slug}-#{version}"

          cached = ::Nexus::Installer.cached_archive(@game_domain, mod_id.to_i, file) do
            ::Nexus::Installer.pick_url(@client.download_urls(@game_domain, mod_id.to_i, file_id))
          end

          tmp = Dir.mktmpdir("nexus-")
          ::Nexus::Installer.extract(cached, tmp)
          FileUtils.chmod_R(0o755, tmp)

          source = {
            "provider"      => "nexus",
            "game_domain"   => @game_domain,
            "mod_id"        => mod_id.to_i,
            "file_id"       => file["file_id"],
            "category_name" => file["category_name"],
            "uploaded_at"   => file["uploaded_timestamp"].to_i,
          }

          UnpackedMod.new(
            tmp_dir: tmp,
            slug:,
            version:,
            game:    @game_domain,
            name:    info["name"],
            source:,
          )
        end

        private

        def slugify(name)
          name.downcase.gsub(/[^a-z0-9]+/, "-").gsub(/\A-+|-+\z/, "")
        end
      end
    end
  end
end
