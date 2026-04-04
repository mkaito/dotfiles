-- This will run last in the setup process.
-- This is just pure lua so anything that doesn't
-- fit in the normal config locations above can go here
--

-- Prepend mise shims to PATH
-- vim.env.PATH = vim.env.HOME .. "/.local/share/mise/shims:" .. vim.env.PATH

-- Hyprlang filetype
vim.filetype.add {
  pattern = { [".*/hypr/.*%.conf"] = "hyprlang" },
}
