# frozen_string_literal: true

require "minitest/autorun"
require_relative "../../lib/mod_manager/topo_sort"
require_relative "../../lib/mod_manager/mod"

include ModManager

class TopoSortTest < Minitest::Test
  def test_linear_chain
    mods = [stub_mod("c", %w[b]), stub_mod("b", %w[a]), stub_mod("a")]
    assert_equal %w[a b c], TopoSort.sort(mods).map(&:slug)
  end

  def test_no_deps_preserves_input_order
    mods = [stub_mod("x"), stub_mod("y"), stub_mod("z")]
    assert_equal %w[x y z], TopoSort.sort(mods).map(&:slug)
  end

  def test_cycle_raises
    mods = [stub_mod("a", %w[b]), stub_mod("b", %w[a])]
    assert_raises(TopoSort::CycleError) { TopoSort.sort(mods) }
  end

  def test_dep_not_in_set_passes_through
    mods = [stub_mod("a", %w[external])]
    assert_equal %w[a], TopoSort.sort(mods).map(&:slug)
  end

  private

  def stub_mod(slug, depends = [])
    Mod.new(slug:, version: "1.0", name: slug, game: "cp2077",
            depends:, source: {}, path: "/stub/#{slug}/1.0")
  end
end
