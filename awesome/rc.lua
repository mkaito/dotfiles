-- {{{ License
-- 
-- Awesome configuration for awesome v3.4.5 (Close To You).
--   * M Kaito <chris.webstar@gmail.com>
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
-- Widgets
require("vicious")
require("vicious.contrib.pulse")

-- {{{ Variable definitions
local terminal = "urxvtc"
local editor_cmd = terminal .. "ect"

local altkey = "Mod1"
local modkey = "Mod4"

local tbheight = 12
local wbheight = tbheight

local home   = os.getenv("HOME")
local exec   = awful.util.spawn
local sexec  = awful.util.spawn_with_shell

-- Beautiful theme
--- beautiful.init(home .. "/.config/awesome/zenburn.lua")
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
  layout = { layouts[1], layouts[1], layouts[3], layouts[3], layouts[1],
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

-- {{{ Battery state
baticon = widget({ type = "imagebox" })
baticon.image = image(beautiful.widget_bat)
-- Initialize widget
batwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(batwidget, vicious.widgets.bat, "$1$2%", 61, "BAT0")
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
vicious.cache(vicious.widgets.pulse)
---- Register widgets
vicious.register(volwidget, vicious.contrib.pulse,
		 function(widget, v)
		    local volume = math.floor(v[1])
		    return " " .. volume .. "%"
		 end,
		 2, "alsa_output.pci-0000_00_1b.0.analog-stereo")
vicious.register(volbar,    vicious.contrib.pulse, "$1",  2, "alsa_output.pci-0000_00_1b.0.analog-stereo")

--vicious.register(volbar,    vicious.widgets.volume,  "$1",  2, "Master")
--vicious.register(volwidget, vicious.widgets.volume, " $1%", 2, "Master")
---- Register buttons
volbar.widget:buttons(awful.util.table.join(
  awful.button({ }, 1, function () awful.util.spawn("pavucontrol") end),
  awful.button({ }, 4, function () vicious.contrib.pulse.add(5, "alsa_output.pci-0000_00_1b.0.analog-stereo") end),
  awful.button({ }, 5, function () vicious.contrib.pulse.add(-5, "alsa_output.pci-0000_00_1b.0.analog-stereo") end)
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
  files = { home.."/.org/notes.org", home.."/.org/personal.org", home.."/.org/work.org"
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
        separator, orgwidget,  orgicon,
        separator, batwidget, baticon,
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
    -- awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    -- awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Shortcuts for multimedia keyboard and Print Screen
    awful.key({                   }, "#121",  function () sexec("~/dev/bin/pvol.py -m", false) end),
    awful.key({                   }, "#122",  function () sexec("~/dev/bin/pvol.py -c -2", false) end),
    awful.key({                   }, "#123",  function () sexec("~/dev/bin/pvol.py -c 2", false) end),
    awful.key({                   }, "Print", function () sexec("scrot -q 100 -e 'mv $f ~/screenshots/ 2>/dev/null'") end),

    -- Awesome base control
    awful.key({ modkey, "Control" }, "r",      awesome.restart),
    awful.key({ modkey, "Shift"   }, "q",      awesome.quit),

    -- {{{ Applications
    awful.key({ modkey            }, "e",      function () exec("emacsclient -n -c") end),
    awful.key({ modkey            }, "t",      function () exec("nautilus --no-desktop", false) end),
--  awful.key({ modkey            }, "F12",    function () sexec("java -jar ~/Descargas/JDownloader/JDownloader.jar") end),
    awful.key({ modkey            }, "F1",     function () exec(terminal) end),
    awful.key({ modkey            }, "F2",     function () sexec("env GTK2_RC_FILES=/usr/share/themes/A-New-Hope/gtk-2.0/gtkrc firefox") end),
    awful.key({ modkey            }, "F3",     function () exec("emacs --name=twmode -e twit") end),
--  awful.key({ modkey            }, "F4",     function () exec("emacs --name=gnus -e gnus") end),
--  awful.key({ altkey            }, "#49",    function () scratch.drop("urxvt", "bottom") end),
    awful.key({ modkey            }, "F4",      function () exec("urxvtc -T Alpine -e alpine") end),
--  awful.key({ modkey            }, "r",      function () exec("urxvtc -T Snownews -e snownews") end),
--  awful.key({ modkey            }, "g",      function () sexec("GTK2_RC_FILES=~/.gtkrc-gajim gajim") end),
--  awful.key({ modkey            }, "g",      function () sexec("pidgin") end),
    awful.key({ modkey,           }, "g",      function () sexec("urxvt -T Bitlbee -e screen -l -UDRS Bitlbee irssi -c bitlbee") end),
    awful.key({ modkey            }, "q",      function () exec("emacsclient --eval '(make-remember-frame)'") end),
    awful.key({ modkey            }, "s",      function () exec("sonata", false) end),

    awful.key({ modkey            }, "F11",    function ()
						  sexec('WINEPREFIX=/home/chris/.wine_64 wine explorer /desktop=1,1440x888 "C:\EVE\eve.exe"',
						       false)
					       end),

    awful.key({ modkey            }, "F12",    function ()
						  sexec('WINEPREFIX=/home/chris/.wine_64 wine explorer /desktop=0,1440x888 "C:\EVE\eve.exe"',
						       false)
					       end),
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
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
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

    { rule = { class = "gimp" },          properties = { floating = true  } },
    { rule = { name  = "Alpine" },        properties = { tag = tags[1][4] } },

    { rule = { name  = "twmode" },        properties = { tag = tags[1][4] } },
    { rule = { name  = "gnus" },        properties = { tag = tags[1][4] } },

    { rule = { class = "Lanikai" },       properties = { tag = tags[1][4] } },
    { rule = { name  = "Mixer" },         properties = { floating = true  } },
    { rule = { class = "Pavucontrol" },   properties = { floating = true  } },
    { rule = { class = "Gajim.py" },      properties = { tag = tags[1][5] } },
    { rule = { class = "Pidgin" },        properties = { tag = tags[1][5] } },
    { rule = { name  = "Bitlbee" },       properties = { tag = tags[1][5] } },
    { rule = { class = "Empathy" },       properties = { tag = tags[1][5] } },
    { rule = { class = "Knode" },         properties = { tag = tags[1][8] } },

    { rule = { class = "Namoroka" },      properties = { tag = tags[1][3] } },
    { rule = { class = "Minefield" },     properties = { tag = tags[1][3] } },
    { rule = { class = "Firefox" },       properties = { tag = tags[1][3], floating = true } },
    { rule = { class = "Chromium" },      properties = { tag = tags[1][3] } },
    { rule = { class = "Epiphany" },      properties = { tag = tags[1][3] } },

    { rule = { class = "Firefox",
	       instance = "Navigator" },  properties = { tag = tags[1][3], floating = false } },
    
    { rule = { class = "Emacs",
	       instance = "emacs" },      properties = { tag = tags[1][2] } },
    { rule = { class = "Emacs",
	       instance = "org" },        properties = { tag = tags[1][7] } },
    { rule = { class = "Gvim" },          properties = { tag = tags[1][2] } },

    { rule = { class = "Wine",
	       instance ="EverQuest2.exe" }, properties = { floating = false } },

    { rule = { class = "Wine",
	       instance ="explorer.exe" }, properties = { floating = false } },

    { rule = { class = "Emacs",
	       instance = "_Remember_" }, properties = { floating = true } },

    { rule = { class = "Xmessage",
	       instance = "xmessage" },   properties = { floating = true } },

    { rule = { name = "Liferea" },        properties = { tag = tags[1][8] } },
    { rule = { class = "Sonata" },        properties = { floating = true, tag = tags[1][9] } },

    { rule=  { class = "sun-awt-X11-XFramePeer",
	       instance = " jd-Main" },   properties = { floating = true, tag = tags[1][9] } },
}
-- }}}

-- {{{ Signals
--
-- {{{ Manage signal handler
client.add_signal("manage", function (c, startup)
    -- Add titlebar to floaters, but remove those from rule callback
    -- if awful.client.floating.get(c)
    -- or awful.layout.get(c.screen) == awful.layout.suit.floating then
    --     if   c.titlebar then awful.titlebar.remove(c)
    --     else awful.titlebar.add(c, {modkey = modkey, height=tbheight}) end
    -- end

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
