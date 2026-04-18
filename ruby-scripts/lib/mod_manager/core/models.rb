# frozen_string_literal: true

module ModManager
  # Temporary mod data produced by the Download port after unpacking.
  # Passed to ModArchive#install; tmp_dir is caller-managed (adapter owns lifetime).
  UnpackedMod = Data.define(:tmp_dir, :slug, :version, :game, :name, :source)

  # A file entry returned by Download#list_files.
  FileInfo = Data.define(:file_id, :name, :version, :category, :description)

  # A post-deploy check declared in config (e.g. assert a dir or file exists).
  Check = Data.define(:path, :type)

  # Per-mod entry from collection.json (inside the collection 7z).
  # choices: nil = no FOMOD pre-selection; Hash = pre-selected FOMOD options.
  CollectionManifestMod = Data.define(:file_id, :mod_id, :name, :phase, :choices)
end
