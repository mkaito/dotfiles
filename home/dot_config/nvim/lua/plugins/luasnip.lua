---@type LazySpec
return {
  {
    "L3MON4D3/LuaSnip",
    config = function(plugin, opts)
      -- include the default astronvim config that calls the setup call
      require "astronvim.plugins.configs.luasnip"(plugin, opts)

      -- load snippets paths
      require("luasnip.loaders.from_lua").lazy_load {
        paths = { vim.fn.stdpath "config" .. "/snippets/lua" },
      }
      require("luasnip.loaders.from_snipmate").lazy_load {
        paths = { vim.fn.stdpath "config" .. "/snippets/snipmate" },
      }
    end,
  },
  -- Defined by AstroNvim as a dependency of LuaSnip, but I don't want it
  {
    "rafamadriz/friendly-snippets",
    enabled = false,
  },
}
