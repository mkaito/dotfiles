# frozen_string_literal: true

# Two test classes in this file:
#
# NexusCollectionFixtureTest  — runs always; uses saved JSON fixtures to test
#                               parsing logic in Nexus::Client + the adapter.
#                               No network, no API key required.
#
# NexusCollectionLiveTest     — skipped unless NEXUS_API_KEY is set.
#                               Hits the real API and asserts responses still
#                               match the saved fixtures (shape + key values).
#                               Run to verify fixtures are still fresh:
#
#   mise exec -- ruby -Ilib spec/system/nexus_collection_spec.rb

require "minitest/autorun"
require "json"
require "nexus/client"
require "mod_manager/collection_revision"
require "mod_manager/adapters/collection_provider/nexus"

FIXTURE_DIR = File.expand_path("../fixtures/nexus", __dir__)
GAME_DOMAIN = "cyberpunk2077"
COLL_SLUG = "n0nymh"
COLL_REV = 47

def fixture(name)
  path = File.join(FIXTURE_DIR, name)
  skip "fixture missing: #{name} — run: rake nexus:download_fixtures" unless File.exist?(path)
  JSON.parse(File.read(path))
end

# ── Fixture-based tests (always run) ──────────────────────────────────────────

class NexusCollectionFixtureTest < Minitest::Test
  include ModManager

  def setup
    @latest = fixture("collection_#{COLL_SLUG}_latest.json")
    @rev47 = fixture("collection_#{COLL_SLUG}_rev#{COLL_REV}.json")
    @revisions = fixture("collection_#{COLL_SLUG}_revisions.json")

    # Stub client: return fixture data for each call signature.
    client = Object.new
    client.define_singleton_method(:collection_revision) do |_game, _slug, revision: nil|
      revision ? @rev47 : @latest
    end.tap {
      client.instance_variable_set(:@rev47, @rev47)
      client.instance_variable_set(:@latest, @latest)
    }
    client.define_singleton_method(:collection_revisions) { |_game, _slug| @revisions }
    client.instance_variable_set(:@revisions, @revisions)

    @adapter = Adapters::CollectionProvider::Nexus.new(GAME_DOMAIN, client)
  end

  # ── adapter: fetch_revision (latest) ──────────────────────────────────────

  def test_fetch_revision_latest_returns_collection_revision
    rev = @adapter.fetch_revision(slug: COLL_SLUG)
    assert_kind_of CollectionRevision, rev
    assert_equal COLL_SLUG, rev.collection_id
    assert_match(/\A[a-z0-9-]+\z/, rev.collection_name)
    assert_kind_of Integer, rev.revision_number
    refute_empty rev.mods
    assert_kind_of CollectionRevisionMod, rev.mods.first
    assert_kind_of Integer, rev.mods.first.mod_id
    assert_kind_of Integer, rev.mods.first.file_id
  end

  # ── adapter: fetch_revision (specific) ────────────────────────────────────

  def test_fetch_revision_specific_returns_correct_revision
    rev = @adapter.fetch_revision(slug: COLL_SLUG, revision: COLL_REV)
    assert_equal COLL_REV, rev.revision_number
    assert_equal "cet-essentials", rev.collection_name
    assert_equal 11, rev.mods.size
    m = rev.mods.first
    refute_nil m.file_name
    refute_nil m.file_version
    refute_nil m.predicted_slug
    assert_match(/\Anexus-\d+-\d+-.+-.+\z/, m.predicted_slug)
  end

  # ── adapter: list_revisions ────────────────────────────────────────────────

  def test_list_revisions_returns_summaries
    summaries = @adapter.list_revisions(slug: COLL_SLUG)
    assert_kind_of Array, summaries
    refute_empty summaries
    s = summaries.first
    assert_kind_of CollectionRevisionSummary, s
    assert_kind_of Integer, s.revision_number
    refute_nil s.status
  end

  # ── raw fixture shape: latest ──────────────────────────────────────────────

  def test_latest_fixture_has_expected_shape
    col = @latest["collection"]
    refute_nil col, "fixture missing 'collection' key"
    assert_equal COLL_SLUG, col["slug"]
    refute_nil col["name"]

    rev = col["latestPublishedRevision"]
    refute_nil rev, "fixture missing 'latestPublishedRevision'"
    assert_kind_of Integer, rev["revisionNumber"]
    assert_kind_of Array, rev_mods(rev)
    first = rev_mods(rev).first
    assert first.key?("fileId"), "mod entry missing fileId"
    assert first["file"].key?("modId"), "mod entry missing file.modId"
    assert first["file"].key?("name"), "mod entry missing file.name"
    assert first["file"].key?("version"), "mod entry missing file.version"
  end

  # ── raw fixture shape: specific revision ──────────────────────────────────

  def test_rev47_fixture_has_expected_shape
    rev = @rev47["collectionRevision"]
    refute_nil rev, "fixture missing 'collectionRevision' key"
    assert_equal COLL_REV, rev["revisionNumber"]
    refute_nil rev.dig("collection", "slug")
    assert_kind_of Array, rev_mods(rev)
    assert_equal 11, rev_mods(rev).size
    first = rev_mods(rev).first
    assert first["file"].key?("name"), "mod entry missing file.name"
    assert first["file"].key?("version"), "mod entry missing file.version"
  end

  # ── raw fixture shape: revisions list ─────────────────────────────────────

  def test_revisions_fixture_has_expected_shape
    revs = @revisions.dig("collection", "revisions")
    assert_kind_of Array, revs, "fixture missing collection.revisions"
    refute_empty revs
    r = revs.first
    assert r.key?("revisionNumber"), "revision entry missing revisionNumber"
    assert r.key?("createdAt"), "revision entry missing createdAt"
    assert r.key?("revisionStatus"), "revision entry missing revisionStatus"
    assert r.key?("modCount"), "revision entry missing modCount"
  end

  private

  def rev_mods(rev)
    rev["modFiles"] || []
  end
end

# ── Live API tests (skipped without NEXUS_API_KEY) ────────────────────────────

class NexusCollectionLiveTest < Minitest::Test
  def setup
    skip "NEXUS_API_KEY not set" unless ENV["NEXUS_API_KEY"]
    @client = Nexus::Client.new(ENV["NEXUS_API_KEY"])
  end

  # Fetch live data and assert it structurally matches the saved fixtures.
  # Failure here means the API changed — re-run rake nexus:download_fixtures
  # and inspect the diff before updating the fixtures.

  def test_latest_matches_fixture_shape
    saved = fixture("collection_#{COLL_SLUG}_latest.json")
    live = @client.collection_revision(GAME_DOMAIN, COLL_SLUG)
    assert_same_keys saved, live, "collection_#{COLL_SLUG}_latest.json"
  end

  def test_rev47_matches_fixture_shape
    saved = fixture("collection_#{COLL_SLUG}_rev#{COLL_REV}.json")
    live = @client.collection_revision(GAME_DOMAIN, COLL_SLUG, revision: COLL_REV)
    assert_same_keys saved, live, "collection_#{COLL_SLUG}_rev#{COLL_REV}.json"
  end

  def test_revisions_matches_fixture_shape
    saved = fixture("collection_#{COLL_SLUG}_revisions.json")
    live = @client.collection_revisions(GAME_DOMAIN, COLL_SLUG)
    assert_same_keys saved, live, "collection_#{COLL_SLUG}_revisions.json"
  end

  private

  # Recursively assert that every key present in +expected+ also exists in
  # +actual+ (at the same path). Ignores extra keys in actual — we only care
  # that our fixture keys still work. Digs into the first element of arrays.
  def assert_same_keys(expected, actual, label, path = [])
    return unless expected.is_a?(Hash) && actual.is_a?(Hash)
    expected.each_key do |k|
      full_path = (path + [k]).join(".")
      assert actual.key?(k), "#{label}: key '#{full_path}' missing from live API response (API may have changed)"
      e_val = expected[k]
      a_val = actual[k]
      if e_val.is_a?(Hash)
        assert_same_keys(e_val, a_val, label, path + [k])
      elsif e_val.is_a?(Array) && e_val.first.is_a?(Hash) && a_val.is_a?(Array) && a_val.first.is_a?(Hash)
        assert_same_keys(e_val.first, a_val.first, label, path + [k, "[]"])
      end
    end
  end
end
