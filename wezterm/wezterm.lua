local wezterm = require("wezterm")
local mux = wezterm.mux
local act = wezterm.action

local config = wezterm.config_builder()
-- local smart_splits = wezterm.plugin.require("https://github.com/mrjones2014/smart-splits.nvim")

wezterm.on("gui-attached", function(_)
	-- maximize all displayed windows on startup
	local workspace = mux.get_active_workspace()
	for _, window in ipairs(mux.all_windows()) do
		if window:get_workspace() == workspace then
			window:gui_window():maximize()
		end
	end
end)

wezterm.on("update-right-status", function(window, _)
	-- Show which key table is active in the status area
	local name = window:active_key_table()
	if name then
		name = "TABLE: " .. name
	end
	window:set_right_status(name or "")
end)

-- config.default_prog = { "/opt/homebrew/bin/fish" }
config.term = "wezterm"
config.color_scheme = "tokyonight_night"
config.audible_bell = "Disabled"

config.font = wezterm.font("Iosevka Custom", { weight = "ExtraLight" })
config.font_size = 16

config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = false
config.use_fancy_tab_bar = false
config.window_decorations = "INTEGRATED_BUTTONS|RESIZE"
config.native_macos_fullscreen_mode = true

config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}

config.window_close_confirmation = "NeverPrompt"

config.keys = {
	{
		key = "f",
		mods = "CTRL|CMD",
		action = act.ToggleFullScreen,
	},

	-- Ctrl-a will drop me in "pane" mode until I press another key or until 1 second elapses
	-- {
	-- 	key = "a",
	-- 	mods = "CTRL",
	-- 	action = act.ActivateKeyTable({
	-- 		name = "pane",
	-- 		timeout_milliseconds = 1000,
	-- 	}),
	-- },
}

-- Tmux-ish split binds
-- config.key_tables = {
-- 	pane = {
-- 		{ key = "-", action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
-- 		{ key = "|", action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }) },
-- 		{ key = "z", action = wezterm.action.TogglePaneZoomState },
--
-- 		-- Extra: clear display
-- 		-- C-l is mapped to pane navigation, so adding C-a C-l as a clear bind
-- 		{
-- 			key = "l",
-- 			mods = "CTRL",
-- 			action = wezterm.action_callback(function(_, pane)
-- 				pane:send_text("\x0c")
-- 			end),
-- 		},
-- 	},
-- }

-- Make sure to put this after any other modifications to config.keys
-- Defaults: C-hjkl for navigation, M-hjkl for resize
-- smart_splits.apply_to_config(config)

return config
