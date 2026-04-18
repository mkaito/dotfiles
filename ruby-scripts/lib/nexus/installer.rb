# frozen_string_literal: true

require "net/http"
require "uri"
require "tmpdir"
require "fileutils"
require "toml-rb"
require_relative "../core/errors"
require_relative "../core/log"
require_relative "../core/file_io"
require_relative "../core/xdg"

module Nexus
  module Installer
    CATEGORY_ORDER = %w[MAIN OPTIONAL MISCELLANEOUS OLD_VERSION].freeze

    # Returns files sorted for display and index-based selection:
    #   - by category (MAIN first, then OPTIONAL, MISCELLANEOUS, OLD_VERSION, others)
    #   - within category: newest first by uploaded_timestamp, tie-break by file_id asc
    def self.sort_files(files)
      files.sort_by do |f|
        cat = CATEGORY_ORDER.index(f["category_name"]) || CATEGORY_ORDER.size
        [cat, -f["uploaded_timestamp"].to_i, f["file_id"].to_i]
      end
    end

    # Returns the single MAIN file if unambiguous; nil if multiple or none.
    def self.auto_select(sorted_files)
      mains = sorted_files.select { _1["category_name"] == "MAIN" }
      mains.first if mains.size == 1
    end

    # Prints a numbered, category-grouped file list to stdout.
    def self.print_file_list(sorted_files, stdout: $stdout)
      current_cat = nil
      sorted_files.each_with_index do |f, i|
        if f["category_name"] != current_cat
          stdout.puts "#{"\n" unless current_cat.nil?}#{f["category_name"]} FILES"
          current_cat = f["category_name"]
        end
        size_mb = (f["size_kb"].to_f / 1024).round(1)
        stdout.puts "  #{i + 1}. #{f["name"]} (#{f["version"]}, #{size_mb} MB)"
      end
      stdout.puts "\nRun again with --file N to download."
    end

    # Downloads and installs a mod. file_id must be resolved before calling.
    # Returns [slug, version].
    def self.install(client, game_domain, mod_id, archive_dir:, file_id:, slug: nil)
      info  = client.mod_info(game_domain, mod_id)
      files = client.mod_files(game_domain, mod_id)
      file  = files.find { _1["file_id"] == file_id } or
              raise Core::Error, "file_id #{file_id} not found for mod #{mod_id}"

      version    = file["version"].to_s.strip.then { _1.empty? ? info["version"].to_s.strip : _1 }
      raise Core::Error, "could not determine version for mod #{mod_id}" if version.empty?
      base_slug  = slugify(file["name"].to_s.sub(/\s+#{Regexp.escape(version)}\z/, ""))
      slug     ||= "nexus-#{mod_id}-#{file["file_id"]}-#{base_slug}-#{version}"

      cached = cached_archive(game_domain, mod_id, file) do
        urls = client.download_urls(game_domain, mod_id, file_id)
        pick_url(urls)
      end

      Dir.mktmpdir("nexus-") do |tmp|
        extract(cached, tmp)
        game_root = detect_root(tmp)

        dest = File.join(archive_dir, game_domain, slug)
        FileUtils.mkdir_p(dest)
        Dir.glob("#{game_root}/*").each { FileUtils.cp_r(_1, dest) }

        meta = {
          "name"    => info["name"],
          "slug"    => slug,
          "version" => version,
          "game"    => game_domain,
          "depends" => [],
          "source"  => {
            "provider"      => "nexus",
            "game_domain"   => game_domain,
            "mod_id"        => mod_id,
            "file_id"       => file["file_id"],
            "category_name" => file["category_name"],
            "uploaded_at"   => file["uploaded_timestamp"].to_i
          }
        }
        Core::FileIO.atomic_write(File.join(dest, "meta.toml"), TomlRB.dump(meta))
      end

      [slug, version]
    end

    private_class_method def self.slugify(name)
      name.downcase.gsub(/[^a-z0-9]+/, "-").gsub(/\A-+|-+\z/, "")
    end

    private_class_method def self.pick_url(urls)
      raise Core::Error, "no download URL returned" if urls.empty?
      preferred = urls.find { _1["short_name"] == "Nexus CDN" }
      (preferred || urls.first).fetch("URI")
    end

    # Returns path to cached archive, downloading via block if not cached.
    private_class_method def self.cached_archive(game_domain, mod_id, file, &url_block)
      path = File.join(Core::XDG.cache_home, "mods", "nexus",
                       game_domain.to_s, mod_id.to_s, file["file_id"].to_s, file["file_name"])
      if File.exist?(path)
        Core::Log.info("cache hit: #{file["file_name"]}")
        return path
      end

      Core::Log.info("downloading #{file["file_name"]}...")
      FileUtils.mkdir_p(File.dirname(path))
      url = url_block.call
      download(url, path)
      path
    end

    private_class_method def self.download(url, dest)
      uri = URI(url.gsub(" ", "%20"))
      Net::HTTP.start(uri.hostname, uri.port, use_ssl: uri.scheme == "https") do |http|
        http.request(Net::HTTP::Get.new(uri)) do |res|
          raise Core::Error, "download failed: HTTP #{res.code}" unless res.is_a?(Net::HTTPSuccess)
          File.open(dest, "wb") { |f| res.read_body { f.write(_1) } }
        end
      end
    end

    private_class_method def self.extract(archive_path, dir)
      case File.extname(archive_path).downcase
      when ".zip" then system("unzip", "-q", "-o", archive_path, "-d", dir)
      when ".7z"  then system("7za", "x", "-y", "-o#{dir}", archive_path)
      when ".rar" then system("unrar", "x", "-y", archive_path, dir)
      else raise Core::Error, "unsupported archive format: #{File.extname(archive_path)}"
      end or raise Core::Error, "extraction failed for #{File.basename(archive_path)}"
    end

    # Strips single wrapper directory if archive contained one.
    private_class_method def self.detect_root(dir)
      entries = Dir.glob("#{dir}/*")
      entries.size == 1 && File.directory?(entries.first) ? entries.first : dir
    end
  end
end
