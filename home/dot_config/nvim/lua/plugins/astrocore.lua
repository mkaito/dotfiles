-- AstroCore provides a central place to modify mappings, vim options, autocommands, and more!
-- Configuration documentation can be found with `:h astrocore`
-- NOTE: We highly recommend setting up the Lua Language Server (`:LspInstall lua_ls`)
--       as this provides autocomplete and documentation while editing

---@type LazySpec
return {
  "AstroNvim/astrocore",
  ---@type AstroCoreOpts
  opts = {
    -- Configure core features of AstroNvim
    features = {
      large_buf = { size = 1024 * 256, lines = 10000 }, -- set global limits for large files for disabling features like treesitter
      autopairs = true, -- enable autopairs at start
      cmp = true, -- enable completion at start
      diagnostics = { virtual_text = true, virtual_lines = false }, -- diagnostic settings on startup
      highlighturl = true, -- highlight URLs at start
      notifications = true, -- enable notifications at start
    },
    -- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
    diagnostics = {
      virtual_text = true,
      underline = true,
    },
    -- treesitter configuration (v6: lives in astrocore, not nvim-treesitter opts)
    treesitter = {
      ensure_installed = { "lua", "vim", "ruby", "yaml" },
    },
    -- vim options can be configured here
    options = {
      opt = { -- vim.opt.<key>
        relativenumber = true, -- sets vim.opt.relativenumber
        number = true, -- sets vim.opt.number
        spell = false, -- sets vim.opt.spell
        signcolumn = "yes", -- sets vim.opt.signcolumn to yes
        wrap = false, -- sets vim.opt.wrap
        showtabline = 0,
        termguicolors = true,
        -- Use ripgrep for :grep
        grepprg = "rg --vimgrep",
        grepformat = "%f:%l:%c:%m",

        -- Remember more old files
        shada = "!,'5000,<50,s10,h",
      },
      g = { -- vim.g.<key>
        -- configure global vim variables (vim.g)
        -- NOTE: `mapleader` and `maplocalleader` must be set in the AstroNvim opts or before `lazy.setup`
        -- This can be found in the `lua/lazy_setup.lua` file
      },
    },
    -- resession.nvim integration (ships with AstroNvim).
    -- Auto-saves a per-cwd session on quit; auto-loaded below on VimEnter.
    sessions = {
      autosave = {
        last = true,
        cwd = true,
      },
      ignore = {
        dirs = {},
        filetypes = { "gitcommit", "gitrebase" },
        buftypes = { "nofile" },
      },
    },
    autocmds = {
      mail_settings = {
        {
          event = "FileType",
          pattern = "mail",
          callback = function()
            vim.opt_local.textwidth = 72
            vim.opt_local.spell = true
            vim.opt_local.wrap = true
          end,
        },
      },
      restore_session = {
        {
          event = "VimEnter",
          desc = "Auto-load resession for cwd when nvim starts with no args",
          nested = true,
          callback = function()
            if vim.fn.argc(-1) == 0 then
              local ok, resession = pcall(require, "resession")
              if ok then
                resession.load(vim.fn.getcwd(), { dir = "dirsession", silence_errors = true })
              end
            end
          end,
        },
      },
    },
    -- Mappings can be configured through AstroCore as well.
    -- NOTE: keycodes follow the casing in the vimdocs. For example, `<Leader>` must be capitalized
    mappings = {
      -- first key is the mode
      n = {
        -- tables with just a `desc` key will be registered with which-key if it's installed
        -- this is useful for naming menus
        ["<Leader>b"] = { desc = "Buffers" },
        ["<Leader><Leader>"] = {
          function() require("snacks").picker.buffers() end,
          desc = "Pick buffers",
        },
        ["<Leader>f<CR>"] = false,
        ["<Leader>f<Leader>"] = {
          function() require("snacks").picker.resume() end,
          desc = "Resume previous search",
        },
        ["<Leader>bp"] = false,
        ["<Leader><TAB>"] = {
          function() require("astrocore.buffer").prev() end,
          desc = "Previous buffer",
        },

        -- second key is the lefthand side of the map
        [";"] = { ":", desc = "Command mode" },

        -- setting a mapping to false will disable it
        ["<C-S>"] = false,
      },
      v = {
        [";"] = { ":", desc = "Command mode" },
      },
    },
  },
}
