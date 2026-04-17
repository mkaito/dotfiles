# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require_relative "../../lib/mod_manager/atom"
require_relative "../../lib/mod_manager/archive"

include ModManager

class AtomParseTest < Minitest::Test
  def test_bare_slug
    a = Atom.parse("redscript")
    assert_equal "redscript", a[:slug]
    assert_equal :latest, a[:op]
    assert_nil a[:version]
  end

  def test_exact
    a = Atom.parse("=redscript-2.1.0")
    assert_equal "redscript", a[:slug]
    assert_equal :eq, a[:op]
    assert_equal "2.1.0", a[:version]
  end

  def test_gte
    a = Atom.parse(">=codeware-2.0")
    assert_equal "codeware", a[:slug]
    assert_equal :gte, a[:op]
    assert_equal "2.0", a[:version]
  end

  def test_hyphenated_slug
    a = Atom.parse("=my-cool-mod-1.0.0")
    assert_equal "my-cool-mod", a[:slug]
    assert_equal "1.0.0", a[:version]
  end

  def test_lt_and_lte
    assert_equal :lt,  Atom.parse("<redscript-3.0")[:op]
    assert_equal :lte, Atom.parse("<=redscript-3.0")[:op]
  end

  def test_invalid_atom_raises
    assert_raises(Error) { Atom.parse("=no-version-here") }
  end
end

class AtomResolveTest < Minitest::Test
  def setup
    @dir = Dir.mktmpdir
    make_mod("redscript", "2.0.0")
    make_mod("redscript", "1.5.0")
    make_mod("codeware",  "3.0.0")
    @archive = Archive.new(@dir)
  end

  def teardown = FileUtils.rm_rf(@dir)

  def test_latest_returns_highest
    assert_equal "2.0.0", Atom.resolve("redscript", @archive).version
  end

  def test_exact_match
    assert_equal "1.5.0", Atom.resolve("=redscript-1.5.0", @archive).version
  end

  def test_gte_returns_highest_satisfying
    assert_equal "2.0.0", Atom.resolve(">=redscript-1.0", @archive).version
  end

  def test_gte_no_match_returns_nil
    assert_nil Atom.resolve(">=redscript-99.0", @archive)
  end

  def test_unknown_slug_returns_nil
    assert_nil Atom.resolve("no-such-mod", @archive)
  end

  def test_gt_returns_first_strictly_greater
    assert_equal "2.0.0", Atom.resolve(">redscript-1.5.0", @archive).version
  end

  def test_gt_no_match_returns_nil
    assert_nil Atom.resolve(">redscript-2.0.0", @archive)
  end

  def test_lte_returns_highest_satisfying
    assert_equal "1.5.0", Atom.resolve("<=redscript-1.5.0", @archive).version
  end

  private

  def make_mod(slug, version)
    dir = File.join(@dir, slug, version)
    FileUtils.mkdir_p(File.join(dir, "files"))
    File.write(File.join(dir, "meta.toml"), <<~TOML)
      name = "#{slug}"
      slug = "#{slug}"
      version = "#{version}"
      game = "cp2077"
      depends = []
    TOML
  end
end
