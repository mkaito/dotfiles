# frozen_string_literal: true

require "rexml/document"
require "core/errors"

module Nexus
  # One selectable option inside a FOMOD SelectExactlyOne group.
  # folders: Array of {"source" => String, "destination" => String}
  FomodChoice = Data.define(:name, :description, :folders)

  module Fomod
    # Returns nil if the extracted dir contains no ModuleConfig.xml.
    # Returns Array<FomodChoice> for the SelectExactlyOne group found.
    # Raises Core::Error if more than one SelectExactlyOne group is present (unsupported).
    def self.detect(extracted_dir)
      xml_path = Dir.glob("#{extracted_dir}/**/[Mm]odule[Cc]onfig.xml").first
      return nil unless xml_path
      parse(xml_path)
    end

    def self.parse(xml_path)
      raw = File.read(xml_path, encoding: "BOM|UTF-16LE:UTF-8")
      doc = REXML::Document.new(raw)

      groups = REXML::XPath.match(doc, "//group[@type='SelectExactlyOne']")
      return nil if groups.empty?
      raise Core::Error, "multiple SelectExactlyOne FOMOD groups not yet supported" if groups.size > 1

      groups.first.elements.to_a("plugins/plugin").map do |plugin|
        name        = plugin.attributes["name"].to_s.strip
        description = plugin.elements["description"]&.text.to_s.strip
        folders     = plugin.elements.to_a("files/folder").map do |f|
          { "source" => f.attributes["source"].to_s, "destination" => f.attributes["destination"].to_s }
        end
        FomodChoice.new(name:, description:, folders:)
      end
    end
  end
end
