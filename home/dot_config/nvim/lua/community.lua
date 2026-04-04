-- AstroCommunity: import any community modules here
-- We import this file in `lazy_setup.lua` before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.

---@type LazySpec
return {
  "AstroNvim/astrocommunity",

  -- Colors
  { import = "astrocommunity.colorscheme.tokyonight-nvim" },

  -- Packs
  { import = "astrocommunity.pack.docker" },
  { import = "astrocommunity.pack.full-dadbod" },
  { import = "astrocommunity.pack.json" },
  { import = "astrocommunity.pack.lua" },
  { import = "astrocommunity.pack.markdown" },
  { import = "astrocommunity.pack.rust" },
  { import = "astrocommunity.pack.terraform" },
  { import = "astrocommunity.pack.typescript" },
  { import = "astrocommunity.pack.yaml" },

  -- Python
  { import = "astrocommunity.pack.python.base" },
  { import = "astrocommunity.pack.python.ruff" },
  { import = "astrocommunity.pack.python.basedpyright" },
}
