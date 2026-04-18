# frozen_string_literal: true

module ModManager
  # A single mod in the archive. `path` is the mod's directory in the archive;
  # adapters use it to enumerate files for deployment.
  Mod = Data.define(:slug, :version, :name, :game, :depends, :source, :path) do
    def to_s = slug
  end

  # An ordered, named list of mod slugs.
  Collection = Data.define(:name, :mods)

  # A deployable set: ordered list of collection names + optional direct mod slugs.
  Modset = Data.define(:name, :game, :collections, :mods, :checks)

  # Temporary mod data produced by the Download port after unpacking.
  # Passed to ModArchive#install; tmp_dir is caller-managed (adapter owns lifetime).
  UnpackedMod = Data.define(:tmp_dir, :slug, :version, :game, :name, :source)

  # A file entry returned by Download#list_files.
  FileInfo = Data.define(:file_id, :name, :version, :category, :description)

  # A post-deploy check declared in config (e.g. assert a dir or file exists).
  Check = Data.define(:path, :type)
end
