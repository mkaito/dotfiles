-- {{{ License
--
-- Awesome configuration for awesome v3.4.10 (Exploder).
--   * M Kaito <me@mkaito.com>
--
-- Large chunks from Adrian C. <anrxc@sysphere.org>
--
-- This work is licensed under the Creative Commons Attribution-Share
-- Alike License: http://creativecommons.org/licenses/by-sa/3.0/
-- }}}

-- Can has...?
-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")
naughty.config.default_preset.screen = mouse.screen
-- Widgets
require("vicious")
-- require("mpd_socket")

-- {{{ Variable definitions
local terminal = "urxvtc "
local editor_cmd = terminal .. "ect"

local altkey = "Mod1"
local modkey = "Mod4"

local tbheight = 12
local wbheight = tbheight

local home   = os.getenv("HOME")
local exec   = awful.util.spawn
local sexec  = awful.util.spawn_with_shell

-- Beautiful theme
beautiful.init(home .. "/.config/awesome/zenburn.lua")

-- Window management layouts
layouts = {
   awful.layout.suit.tile,        -- 1
   awful.layout.suit.tile.bottom, -- 2
   awful.layout.suit.fair,        -- 3
   awful.layout.suit.max,         -- 4
   awful.layout.suit.magnifier,   -- 5
   awful.layout.suit.floating     -- 6
}
-- }}}


-- {{{ Tags
tags = {
   names  = { "term", "emacs", "web", "mail", "im", "boxing", "gaming", "video", "misc" },
   layout = { layouts[1], layouts[1], layouts[3], layouts[2], layouts[1],
              layouts[4], layouts[4], layouts[4], layouts[6]
        }}

for s = 1, screen.count() do
   tags[s] = awful.tag(tags.names, s, tags.layout)
   awful.tag.setproperty(tags[s][5], "mwfact", 0.18)
   --  awful.tag.setproperty(tags[s][6], "hide",   true)
end
-- }}}

-- {{{ Wibox

-- {{{ Widgets configuration

-- {{{ Reusable separator
separator = widget({ type = "imagebox" })
separator.image = image(beautiful.widget_sep)
-- }}}

---- {{{ Volume level
volicon = widget({ type = "imagebox" })
volicon.image = image(beautiful.widget_vol)
---- Initialize widgets
volbar    = awful.widget.progressbar()
volwidget = widget({ type = "textbox" })
---- Progressbar properties
volbar:set_vertical(true):set_ticks(true)
volbar:set_height(12):set_width(8):set_ticks_size(2)
volbar:set_background_color(beautiful.fg_off_widget)
volbar:set_gradient_colors({ beautiful.fg_widget,
                             beautiful.fg_center_widget, beautiful.fg_end_widget
                          }) -- Enable caching
vicious.cache(vicious.widgets.volume)
---- Register widgets
vicious.register(volbar,    vicious.widgets.volume,  "$1",  2, "Master")
vicious.register(volwidget, vicious.widgets.volume, " $1%", 2, "Master")
---- Register buttons
volbar.widget:buttons(awful.util.table.join(
                         awful.button({ }, 1, function () sexec("amixer -q set Master toggle") end),
                         awful.button({ }, 3, function () sexec("urxvtc -e alsamixer") end),
                         awful.button({ }, 4, function () sexec("amixer -q set Master 1+ unmute") end),
                         awful.button({ }, 5, function () sexec("amixer -q set Master 1- unmute") end)
                   )) -- Register assigned buttons
volwidget:buttons(volbar.widget:buttons())
---- }}}
--
-- {{{ Date and time
dateicon = widget({ type = "imagebox" })
dateicon.image = image(beautiful.widget_date)
-- Initialize widget
datewidget = widget({ type = "textbox" })
-- Register widget
vicious.register(datewidget, vicious.widgets.date, "%B %d %Y %H:%M", 61)
-- Register buttons
datewidget:buttons(awful.util.table.join(
                      awful.button({ }, 1, function () exec("/home/chris/src/bin/pylendar.py") end)
                ))
-- }}}

-- {{{ System tray
systray = widget({ type = "systray" })
-- }}}

-- {{{ Org-mode agenda
orgicon = widget({ type = "imagebox" })
orgicon.image = image(beautiful.widget_org)
-- Initialize widget
orgwidget = widget({ type = "textbox" })
-- Configure widget
local orgmode = {
   files = { home.."/.org/personal.org"
          },
   color = {
      past   = '<span color="'..beautiful.fg_urgent..'">',
      today  = '<span color="'..beautiful.fg_normal..'">',
      soon   = '<span color="'..beautiful.fg_widget..'">',
      future = '<span color="'..beautiful.fg_netup_widget..'">'
}} -- Register widget
vicious.register(orgwidget, vicious.widgets.org,
                 orgmode.color.past..'$1</span>-'..orgmode.color.today .. '$2</span>-' ..
                    orgmode.color.soon..'$3</span>-'..orgmode.color.future.. '$4</span>', 601,
                 orgmode.files
              ) -- Register buttons
orgwidget:buttons(awful.util.table.join(
                     awful.button({ }, 1, function () exec("emacsclient --eval '(org-agenda-list)'") end),
                     awful.button({ }, 3, function () exec("emacsclient --eval '(make-remember-frame)'") end)
               ))
-- }}}

-- {{{ Tasklist
tasklist = {}
tasklist.buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
                           if not c:isvisible() then
                              awful.tag.viewonly(c:tags()[1])
                           end
                           client.focus = c
                           c:raise()
                        end),
   awful.button({ }, 3, function ()
                           if instance then
                              instance:hide()
                              instance = nil
                           else
                              instance = awful.menu.clients({ width=250 })
                           end
                        end),
   awful.button({ }, 4, function ()
                           awful.client.focus.byidx(1)
                           if client.focus then client.focus:raise() end
                        end),
   awful.button({ }, 5, function ()
                           awful.client.focus.byidx(-1)
                           if client.focus then client.focus:raise() end
                        end))
-- }}}

-- {{{ MPD widget
mpdicon = widget({ type = "imagebox" })
mpdicon.image = image(beautiful.widget_mpd)
-- Initialize widget
mpdwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(mpdwidget,
                 -- Widget worker
                 function()
                    local function format_time(s)
                       local d = os.date("*t", tonumber(s))
                       return string.format("%d:%02d", d.min, d.sec)
                    end

                    local socket = { unix = require("socket.unix") }
                    local helpers = require("vicious.helpers")

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

                    local mpdsock = socket.unix()
                    mpdsock:connect(os.getenv("MPD_HOST"))
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
                          elseif k == "elapsed" then mpd_state["{elapsed}"] = v and format_time(v)
                          elseif k == "Time" then mpd_state["{length}"] = v and format_time(v)
                          end
                       end
                    end

                    mpdsock:close()
                    return mpd_state
                 end,
                 -- Widget format function
                 function(widget, args)
                    if args["{state}"] == "Stop" then return " -- "
                    else return args["{Artist}"]..' - '..args["{Title}"].." "..
                          args["{elapsed}"].."/"..args["{length}"]
                    end
                 end, 1, {nil, "~/.mpd/mpd.sock", nil})
-- Register buttons
mpdwidget:buttons(awful.util.table.join(
                     awful.button({ }, 1, function () exec("mpc toggle") end),
		     awful.button({ }, 3, function () sexec(terminal.."-T MPD -e ncmpc") end)
               ))
-- }}} MPD widget
-- {{{ Network widget
netdicon = widget({ type = "imagebox" })
netdicon.image = image(beautiful.widget_net)
netdwidget = widget({ type = "textbox" })
vicious.register(netdwidget, vicious.widgets.net, "${eth0 down_kb}Kb/s", 2)

netdgraph = awful.widget.graph()
netdgraph:set_width(20)
netdgraph:set_background_color(beautiful.bg_widget)
netdgraph:set_color(beautiful.fg_widget)
netdgraph:set_gradient_colors({ beautiful.fg_widget, beautiful.fg_center_widget, beautiful.fg_end_widget })
netdgraph:set_gradient_angle(180)
vicious.register(netdgraph, vicious.widgets.net,  function(widget, args)
						     -- Max download pipe capacity is 400Kbps
						     return (tonumber(args["{eth0 down_kb}"]) / 400) * 100
						 end
		 , 2)


netuicon = widget({ type = "imagebox" })
netuicon.image = image(beautiful.widget_netup)
netuwidget = widget({ type = "textbox" })
vicious.register(netuwidget, vicious.widgets.net, "${eth0 up_kb}Kb/s", 2)

netupgraph = awful.widget.graph()
netupgraph:set_width(20)
netupgraph:set_background_color(beautiful.bg_widget)
netupgraph:set_color(beautiful.fg_widget)
netupgraph:set_gradient_colors({ beautiful.fg_widget, beautiful.fg_center_widget, beautiful.fg_end_widget })
netupgraph:set_gradient_angle(180)
vicious.register(netupgraph, vicious.widgets.net,  function(widget, args)
						     -- Max upload pipe capacity is 32Kbps
						     return (tonumber(args["{eth0 up_kb}"]) / 32) * 100
						 end
		 , 2)

-- }}} Network widget

-- {{{ CPU Graph
cpuicon = widget({ type = "imagebox" })
cpuicon.image = image(beautiful.widget_cpu)

cpuwidget = awful.widget.graph()
cpuwidget:set_width(50):set_max_value(100)
cpuwidget:set_scale(true)
cpuwidget:set_background_color(beautiful.bg_widget)
cpuwidget:set_color(beautiful.fg_widget)
cpuwidget:set_gradient_colors({ beautiful.fg_widget, beautiful.fg_center_widget, beautiful.fg_end_widget })
cpuwidget:set_gradient_angle(180)
vicious.register(cpuwidget, vicious.widgets.cpu, function(widget, args)
						    return tonumber(args["1"])
						 end
		 , 3)

-- cputxtwidget = widget({ type = "textbox" })
-- vicious.register(cputxtwidget, vicious.widgets.cpu, "$1%", 3)
-- }}} CPU Graph

-- {{{ Mem bar
memicon = widget({ type = "imagebox" })
memicon.image = image(beautiful.widget_mem)

memwidget = awful.widget.progressbar()
memwidget:set_width(8):set_height(12):set_ticks_size(2)
memwidget:set_vertical(true):set_ticks(true)
memwidget:set_background_color(beautiful.fg_off_widget)
memwidget:set_border_color(nil)
memwidget:set_color(beautiful.fg_widget)
memwidget:set_gradient_colors({ beautiful.fg_widget, beautiful.fg_center_widget, beautiful.fg_end_widget })
vicious.register(memwidget, vicious.widgets.mem, "$1", 15)

-- memtxtwidget = widget({ type = "textbox" })
-- vicious.register(memtxtwidget, vicious.widgets.mem, "$1% ($2MB/$3MB)", 13)
-- }}} Mem bar

-- }}}

-- {{{ Wibox initialisation
wibox     = {}
promptbox = {}
layoutbox = {}
taglist   = {}
taglist.buttons = awful.util.table.join(
   awful.button({ },        1, awful.tag.viewonly),
   awful.button({ modkey }, 1, awful.client.movetotag),
   awful.button({ },        3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, awful.client.toggletag),
   awful.button({ },        4, awful.tag.viewnext),
   awful.button({ },        5, awful.tag.viewprev
          ))

for s = 1, screen.count() do
   -- Create a promptbox
   promptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
   -- Create a layoutbox
   layoutbox[s] = awful.widget.layoutbox(s)
   layoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts,  1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts,  1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)
                     ))

   -- Create the taglist
   taglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, taglist.buttons)
   -- Create the Tasklist
   tasklist[s] = awful.widget.tasklist(function(c)
                                          -- return awful.widget.tasklist.label.currenttags(c, s)
                                          return awful.widget.tasklist.label.focused(c, s)
                                       end, tasklist.buttons)
   -- Create the wibox
   wibox[s] = awful.wibox({      screen = s,
                                 fg = beautiful.fg_normal, height = wbheight,
                                 bg = beautiful.bg_normal, position = "top",
                              })
   -- Add widgets to the wibox
   wibox[s].widgets = {
      {   taglist[s], layoutbox[s], separator, promptbox[s],
          ["layout"] = awful.widget.layout.horizontal.leftright
       },
      s == screen.count() and systray or nil,
      separator, datewidget, dateicon,
      separator, volwidget,  volbar.widget, volicon,
      -- separator, orgwidget,  orgicon,
      --separator, memwidget.widget, memicon,
      --cpuwidget.widget, cpuicon,
      -- separator, netuwidget, netupgraph.widget, netuicon, netdwidget, netdgraph.widget, netdicon,
      --separator, netupgraph.widget, netuicon, netdgraph.widget, netdicon,
      separator, mpdwidget, mpdicon,
      separator, tasklist[s], ["layout"] = awful.widget.layout.horizontal.rightleft
   }
end
-- }}}

-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
          ))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

   awful.key({ modkey,           }, "j",
             function ()
                awful.client.focus.byidx( 1)
                if client.focus then client.focus:raise() end
             end),
   awful.key({ modkey,           }, "k",
             function ()
                awful.client.focus.byidx(-1)
                if client.focus then client.focus:raise() end
             end),
   awful.key({ altkey            },  "Tab",
             function ()
                -- If you want to always position the menu on the same place set coordinates
                awful.menu.menu_keys.down = { "Down", "Alt_L" }
                local cmenu = awful.menu.clients({width=245}, { keygrabber=true, coords={x=525, y=330} })
             end),

   -- Layout manipulation
   awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
   awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
   awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
   awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
   awful.key({ modkey,           }, "Tab",
             function ()
                awful.client.focus.history.previous()
                if client.focus then
                   client.focus:raise()
                end
             end),

   -- Shortcuts for multimedia keyboard and Print Screen
   awful.key({                   }, "#121",  function () sexec("amixer set Master toggle", false) end),
   awful.key({                   }, "#122",  function () sexec("amixer set Master 10- unmute", false) end),
   awful.key({                   }, "#123",  function () sexec("amixer set Master 10+ unmute", false) end),

   awful.key({                   }, "#171",  function () sexec("mpc next", false) end),
   awful.key({                   }, "#172",  function () sexec("mpc toggle", false) end),
   awful.key({                   }, "#173",  function () sexec("mpc next", false) end),
   awful.key({                   }, "#174",  function () sexec("mpc stop", false) end),

   awful.key({                   }, "Print", function () sexec("scrot -q 100 -e 'mv $f ~/screenshots/ 2>/dev/null'") end),
   awful.key({ "Shift",          }, "Print", function () sexec("scrot -q 100 -s -e 'mv $f ~/dev/mydropbox/sync/public/screenshots/ 2>/dev/null'") end),

   -- Awesome base control
   awful.key({ modkey, "Control" }, "r",      awesome.restart),
   awful.key({ modkey, "Shift"   }, "q",      awesome.quit),

   -- {{{ Applications
   awful.key({ modkey            }, "e",      function () sexec("emacsclient -nc") end),
   awful.key({ modkey            }, "t",      function () sexec(terminal.."-T Ranger -e ranger") end),
   awful.key({ modkey            }, "F1",     function () exec(terminal) end),
   awful.key({ modkey            }, "F2",     function () sexec("GTK2_RC_FILES=/usr/share/themes/Aurora/gtk-2.0/gtkrc firefox") end),
   -- awful.key({ modkey            }, "F2",     function () sexec("chromium --enable-accelerated-compositing --memory-model=low") end),
   -- awful.key({ modkey,           }, "F3",     function () sexec("urxvtc -T Twitter -e screen -l -UDRS Twitter /home/chris/dev/bin/ttytter") end),
   -- awful.key({ modkey            }, "F3",     function () sexec("emacs --name twmode -e twit") end),
   awful.key({ modkey            }, "F4",     function () sexec(terminal.."-T Mail -e mutt") end),
   awful.key({ modkey,           }, "g",      function () sexec(terminal.."-T Weechat -e screen -l -UDRS weechat weechat-curses") end),
   awful.key({ modkey            }, "q",      function () sexec("emacsclient --eval '(make-remember-frame)'") end),
   awful.key({ modkey            }, "s",      function () exec(terminal.."-T MPD -e ncmpc", false) end),
   -- }}}

   awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
   awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
   awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
   awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
   awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
   awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
   awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

   -- Prompt
   awful.key({ modkey },            "r",     function () promptbox[mouse.screen]:run() end),

   awful.key({ modkey }, "x",
             function ()
                awful.prompt.run({ prompt = "Run Lua code: " },
                                 mypromptbox[mouse.screen].widget,
                                 awful.util.eval, nil,
                                 awful.util.getdir("cache") .. "/history_eval")
             end)
)

clientkeys = awful.util.table.join(
   awful.key({ modkey, "Shift"   }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
   awful.key({ modkey,           }, "Escape", function (c) c:kill()                         end),
   awful.key({ modkey, "Shift"   }, "space",  awful.client.floating.toggle                     ),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
   awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
   awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
   awful.key({ modkey,           }, "n",      function (c) c.minimized = not c.minimized    end),
   awful.key({ modkey,           }, "m",
             function (c)
                c.maximized_horizontal = not c.maximized_horizontal
                c.maximized_vertical   = not c.maximized_vertical
             end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
   globalkeys = awful.util.table.join(globalkeys,
                                      awful.key({ modkey }, "#" .. i + 9,
                                                function ()
                                                   local screen = mouse.screen
                                                   if tags[screen][i] then
                                                      awful.tag.viewonly(tags[screen][i])
                                                   end
                                                end),
                                      awful.key({ modkey, "Control" }, "#" .. i + 9,
                                                function ()
                                                   local screen = mouse.screen
                                                   if tags[screen][i] then
                                                      awful.tag.viewtoggle(tags[screen][i])
                                                   end
                                                end),
                                      awful.key({ modkey, "Shift" }, "#" .. i + 9,
                                                function ()
                                                   if client.focus and tags[client.focus.screen][i] then
                                                      awful.client.movetotag(tags[client.focus.screen][i])
                                                   end
                                                end),
                                      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                                                function ()
                                                   if client.focus and tags[client.focus.screen][i] then
                                                      awful.client.toggletag(tags[client.focus.screen][i])
                                                   end
                                                end))
end

clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
                    border_color = beautiful.border_normal,
                    focus = true,
                    keys = clientkeys,
                    buttons = clientbuttons } },

   { rule = { class    = "gimp" },          properties = { floating = true  } },
   { rule = { class    = "Galculator" },    properties = { floating = true  } },
   { rule = { name     = "Mail" },          properties = { tag = tags[1][4] } },
   { rule = { name     = "Mixer" },         properties = { floating = true  } },
   { rule = { name     = "Weechat" },       properties = { tag = tags[1][5] } },
   { rule = { name     = "Gajim.py" },      properties = { tag = tags[1][5] } },
   { rule = { class    = "Chromium" },      properties = { tag = tags[1][3] } },
   { rule = { class    = "Firefox" },       properties = { floating = true } },
   { rule = { class    = "Firefox",
           instance    = "Navigator" },     properties = { tag = tags[2][3], floating = false } },
   
   { rule = { class    = "Emacs",
              instance = "emacs" },         properties = { tag = tags[2][2] } },
   { rule = { class    = "Gvim" },          properties = { tag = tags[2][2] } },
   { rule = { class    = "Emacs",
              instance = "_Remember_" },    properties = { floating = true } },
   
   { rule = { class    = "Xmessage",
              instance = "xmessage" },      properties = { floating = true } },
   
   { rule = { class    = "Sonata" },        properties = { floating = true } },
   { rule = { name     = "MPD" },           properties = { floating = true } },
   { rule = { class    = "Anki" },          properties = { floating = true } },
}
-- }}}

-- {{{ Signals
--
-- {{{ Manage signal handler
client.add_signal("manage", function (c, startup)
                               -- Add titlebar to floaters, but remove those from rule callback
                               if awful.client.floating.get(c)
                               or awful.layout.get(c.screen) == awful.layout.suit.floating then
                                   if   c.titlebar then awful.titlebar.remove(c)
                                   else awful.titlebar.add(c, {modkey = modkey, height=tbheight}) end
                               end

                               -- Enable sloppy focus
                               c:add_signal("mouse::enter", function (c)
                                                               if  awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
                                                               and awful.client.focus.filter(c) then
                                                               client.focus = c
                                                            end
                                                         end)

                               -- Client placement
                               if not startup then
                                  awful.client.setslave(c)

                                  if  not c.size_hints.program_position
                                     and not c.size_hints.user_position then
                                     awful.placement.no_overlap(c)
                                     awful.placement.no_offscreen(c)
                                  end
                               end
                            end)
-- }}}

-- {{{ Focus signal handlers
client.add_signal("focus",   function (c) c.border_color = beautiful.border_focus  end)
client.add_signal("unfocus", function (c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{{ Arrange signal handler
for s = 1, screen.count() do screen[s]:add_signal("arrange", function ()
                                                                local clients = awful.client.visible(s)
                                                                local layout = awful.layout.getname(awful.layout.get(s))

                                                                for _, c in pairs(clients) do -- Floaters are always on top
                                                                   if   awful.client.floating.get(c) or layout == "floating"
                                                                   then if not c.fullscreen then c.above       =  true  end
                                                                   else                          c.above       =  false end
                                                                end
                                                             end)
end
-- }}}

-- }}}
