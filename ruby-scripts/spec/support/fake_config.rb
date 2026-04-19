# frozen_string_literal: true

FakeConfig = Struct.new(:archive_dir, :collections_dir, :modsets_dir,
  :game_dir, :domain, :checks, keyword_init: true) do
  def initialize(**kw)
    kw[:checks] ||= []
    super
  end
end
