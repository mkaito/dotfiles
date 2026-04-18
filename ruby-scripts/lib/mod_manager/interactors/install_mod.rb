# frozen_string_literal: true

require "fileutils"
require "mod_manager/errors"

module ModManager
  module Interactors
    class InstallMod
      def initialize(download:, archive:, terminal:)
        @download = download
        @archive  = archive
        @terminal = terminal
      end

      def call(mod_id, file_id:, slug: nil)
        unpacked = @download.fetch(mod_id:, file_id:, slug:)
        mod      = @archive.install(unpacked_mod: unpacked)
        @terminal.success("archived #{mod.slug}")
        mod
      ensure
        FileUtils.rm_rf(unpacked.tmp_dir) if unpacked
      end
    end
  end
end
