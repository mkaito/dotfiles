---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, Adrian C. <anrxc@sysphere.org>
---------------------------------------------------

-- {{{ Grab environment
local print = print
local os = { getenv = os.getenv, date = os.date }
local tonumber = tonumber
local setmetatable = setmetatable
local string = { gmatch = string.gmatch }
local helpers = require("vicious.helpers")
local socket = require("socket")
socket.unix = require("socket.unix")
-- }}}


-- Mpd: provides Music Player Daemon information
module("vicious.widgets.mpd.socket")

local function format_time(secs)
   local d = os.date("*t", tonumber(secs))
   return d["min"]..":"..d["sec"]
end
   

-- {{{ MPD widget type
local function worker(format, warg)
    local mpd_state  = {
        ["{volume}"] = 0,
        ["{state}"]  = "N/A",
        ["{Artist}"] = "N/A",
        ["{Title}"]  = "N/A",
        ["{Album}"]  = "N/A",
        ["{Genre}"]  = "N/A",
        ["{Name}"]   = "N/A",
        ["{file}"]   = "N/A",
	["{elapsed}"]= "N/A",
	["{length}"] = "N/A",
    }

    -- Fallback to MPD defaults
    local host = warg and (warg.host or warg[1]) or os.getenv("MPD_HOST")
    local port = warg and (warg.port or warg[2]) or os.getenv("MPD_PORT")

    if port == "-1" then
       mpdsock = socket.unix()
       mpdsock:connect(host)
    else
       mpdock = socket.connect(host, port)
    end
       
    mpdsock:send("status\ncurrentsong\nclose\n")
    while true do
       local f = mpdsock:receive()
       if not f then break end

       for k, v in string.gmatch(f, "([%w]+):[%s](.*)$") do
       	  if     k == "volume" then mpd_state["{"..k.."}"] = v and tonumber(v)
       	  elseif k == "state"  then mpd_state["{"..k.."}"] = helpers.capitalize(v)
       	  elseif k == "Artist" then mpd_state["{"..k.."}"] = helpers.escape(v)
       	  elseif k == "Title"  then mpd_state["{"..k.."}"] = helpers.escape(v)
       	  elseif k == "Album"  then mpd_state["{"..k.."}"] = helpers.escape(v)
       	  elseif k == "Genre"  then mpd_state["{"..k.."}"] = helpers.escape(v)
       	  elseif k == "Name" then mpd_state["{"..k.."}"] = helpers.escape(v)
       	  elseif k == "file" then mpd_state["{"..k.."}"] = helpers.escape(v)
	  if k == "elapsed" then mpd_state["{elapsed}"] = v and format_time(v)
	  elseif k == "Time" then mpd_state["{length}"] = v and format_time(v)
       	  end
       end
    end
    
    mpdsock:close()
    return mpd_state
end
-- }}}

setmetatable(_M, { __call = function(_, ...) return worker(...) end })

worker()