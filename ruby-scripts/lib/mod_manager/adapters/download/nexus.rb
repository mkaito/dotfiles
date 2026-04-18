# frozen_string_literal: true

require "tmpdir"
require "fileutils"
require_relative "../../../nexus/file_picker"
require_relative "../../../core/http"
require_relative "../../../core/xdg"
require_relative "../../errors"
require_relative "../../core/models"

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
          ::Nexus::FilePicker.sort_files(@client.mod_files(@game_domain, mod_id.to_i))
        end

        def file_exist?(mod_id:, file_id:)
          @client.mod_files(@game_domain, mod_id.to_i).any? { _1["file_id"] == file_id }
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

          cached = cached_archive(mod_id.to_i, file) do
            pick_url(@client.download_urls(@game_domain, mod_id.to_i, file_id))
          end

          tmp = Dir.mktmpdir("nexus-")
          extract(cached, tmp)
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

        def cached_archive(mod_id, file, &url_block)
          path = File.join(Core::XDG.cache_home, "mods", "nexus",
                           @game_domain.to_s, mod_id.to_s, file["file_id"].to_s, file["file_name"])
          Core::Http.cached_download(path, label: file["file_name"]) { url_block.call }
        end

        def pick_url(urls)
          raise Core::Error, "no download URL returned" if urls.empty?
          preferred = urls.find { _1["short_name"] == "Nexus CDN" }
          (preferred || urls.first).fetch("URI")
        end

        def extract(archive_path, dir)
          if archive_path.end_with?(".rar")
            system("unrar", "x", "-y", "-inul", archive_path, dir + "/") or
              raise Core::Error, "extraction failed for #{File.basename(archive_path)}"
          else
            system("7za", "x", "-y", "-bso0", "-bsp0", "-o#{dir}", archive_path) or
              raise Core::Error, "extraction failed for #{File.basename(archive_path)}"
          end
        end

        def detect_root(dir)
          entries = Dir.glob("#{dir}/*")
          entries.size == 1 && File.directory?(entries.first) ? entries.first : dir
        end
      end
    end
  end
end
