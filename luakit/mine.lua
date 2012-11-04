-- Global config
globals.check_filepath = false
globals.homepage = "about:blank"
search_engines.yubnub = "http://yubnub.org/parser/parse?command=%s"
search_engines.default = search_engines.yubnub

-- Lefty follow hints
local s = follow.label_styles
follow.label_maker = s.sort(s.reverse(s.charset("asdfqwerzxcv")))

-- Always save browsing session. no. matter. what.
local close_win = window.methods.close_win
window.methods.close_win = function (w, ...)
	session.save{w}
	close_win(w, ...)
end

-- Open all new window requests in new tabs
webview.init_funcs.window_decision = function (view, w)
	view:add_signal("new-window-decision", function (v, uri, reason)
		w:new_tab(uri)
		return true
	end)
end

-- {{{Download location configuration
downloads.default_dir = os.getenv("HOME") .. "/Downloads"
downloads.add_signal("download-location", function (uri, file)
	if not file or file == "" then
		file = (string.match(uri, "/([^/]+)$")
		or string.match(uri, "^%w+://(.+)")
		or string.gsub(uri, "/", "_")
		or "untitled")
	end

	-- Add filetype specific handlers here
	if file:find(".*%.torrent$") then
		return os.getenv("HOME") .. "/.torrent/watch/" .. file
	end

	return downloads.default_dir .. "/" .. file
end)

domain_props = {
   ["all"] = {
      enable_accelerated_compositing = true,
      enable_page_cache = true,
   },
}
-- }}}

print("mine.lua correctly loaded, it seems.")

-- vim:fdm=marker
