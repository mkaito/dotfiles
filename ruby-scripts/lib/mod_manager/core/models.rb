# frozen_string_literal: true

module ModManager
  # Temporary mod data produced by the Download port after unpacking.
  # Passed to ModArchive#install; tmp_dir is caller-managed (adapter owns lifetime).
  UnpackedMod = Data.define(:tmp_dir, :slug, :version, :game, :name, :source)

  # A file entry returned by Download#list_files.
  FileInfo = Data.define(:file_id, :name, :version, :category, :description)

  # A post-deploy check declared in config (e.g. assert a dir or file exists).
  Check = Data.define(:path, :type)
end
