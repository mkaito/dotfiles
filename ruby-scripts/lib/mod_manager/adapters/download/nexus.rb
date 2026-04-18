# frozen_string_literal: true

require "tmpdir"
require "fileutils"
require "core/text"
require "nexus/file_picker"
require "core/http"
require "mod_manager/adapters/download/translator"
require "core/xdg"
require "mod_manager/errors"
require "mod_manager/core/models"

module ModManager
  module Adapters
    module Download
      class Nexus
        def initialize(game_domain, client)
          @game_domain = game_domain
          @client      = client
        end

        def list_files(mod_id:)
          raw = @client.mod_files(@game_domain, mod_id.to_i)
          ::Nexus::FilePicker.sort_files(raw).map { Translator.file_info(_1) }
        end

        def file_exist?(mod_id:, file_id:)
          @client.download_urls(@game_domain, mod_id.to_i, file_id)
          true
        rescue Core::Error
          false
        end

        # Downloads + unpacks to a temp dir. Caller must clean up unpacked.tmp_dir.
        # Returns UnpackedMod.
        def fetch(mod_id:, file_id:, slug: nil)
          info  = @client.mod_info(@game_domain, mod_id.to_i)
          files = @client.mod_files(@game_domain, mod_id.to_i)
          file  = files.find { _1["file_id"] == file_id } ||
                  @client.mod_file(@game_domain, mod_id.to_i, file_id)

          version   = file["version"].to_s.strip.then { _1.empty? ? info["version"].to_s.strip : _1 }
          raise Core::Error, "could not determine version for mod #{mod_id}" if version.empty?

          base_slug = Core::Text.slugify(file["name"].to_s.sub(/\s+#{Regexp.escape(version)}\z/, ""))
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

      end
    end
  end
end
