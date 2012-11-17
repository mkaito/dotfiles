//See LICENSE file for copyright and license details. 

//appearance 
//static const char font[] = "-xos4-terminus-*-*-*-*-12-*-*-*-*-*-*-u";
static const char font[] = "-xos4-terminus-medium-r-normal-*-14-*-*-*-*-*-*-u";
static const char colors[MAXCOLORS][ColLast][8] = {
	//border     fg         bg       
	{ "#222222", "#7C7C7C", "#1A1A1A" }, /* 0 = normal */
	{ "#876CBE", "#DDDDDD", "#1A1A1A" }, /* 1 = selected */
	{ "#FF2882", "#FFFFFF", "#FF2882" }, /* 2 = urgent */
	{ "#222222", "#53A6A6", "#1A1A1A" }, /* 3 = green */
	{ "#222222", "#BF85CC", "#1A1A1A" }, /* 4 = yellow */
	{ "#222222", "#6096BF", "#1A1A1A" }, /* 5 = cyan */
	{ "#222222", "#7E62B3", "#1A1A1A" }, /* 6 = magenta */
	{ "#222222", "#899CA1", "#1A1A1A" }, /* 7 = grey */
};
static const unsigned int borderpx  = 2;        /* border pixel of windows */
static const unsigned int snap      = 8;        /* snap pixel */
static const Bool showbar           = True;     /* False means no bar */
static const Bool topbar            = True;     /* False means bottom bar */
static const Bool clicktofocus      = True;     /* Change focus only on click */
static const Bool viewontag         = True;     /* Switch view on tag switch */

static const Rule rules[] = {
	//class          instance				title					tags mask     isfloating   iscentred   monitor 
	{ "Gimp",         NULL,					NULL,					0,            True,        False,      -1 },
	{ "MPlayer",      NULL,					NULL,					0,            False,       True,       -1 },
	{ "mplayer2",     NULL,					NULL,					0,            True,        True,       -1 },
	{ "XFontSel",     NULL,					NULL,					0,            True,        True,       -1 },
	{ "Chromium",     NULL,					NULL,					1 << 2,       False,       False,       0 },
	{ "Firefox",      NULL,					NULL,					1 << 2,       True,        True,       -1 },
	{ "Firefox",      "Navigator",	NULL,					1 << 2,       False,       False,       0 },
	{ "Dwb",          NULL,					NULL,					1 << 2,       False,       False,      -1 },
	{ "luakit",       NULL,					NULL,					1 << 2,       False,       False,       0 },
	{ NULL,           NULL,					"JDownloader",1 << 5,	      False,       False,       0 },
	{ "Emacs",        "_Remember_",	NULL,					0,            True,        True,       -1 },
	{ "Anki",         NULL,					NULL,					0,            True,        True,       -1 },
	{ NULL,           NULL,					"Weechat",		1 << 3,       False,       False,       1 },
	{ "Pidgin",       NULL,					NULL,					1 << 3,       False,       False,       1 },
	{ NULL,           NULL,					"Mail",				1 << 3,       False,       False,       1 },
	{ NULL,           NULL,					"mixer",			0,            True,        True,       -1 },
	{ "Pavucontrol",  NULL,					NULL,					0,            True,        True,       -1 },
	{ NULL,           NULL,					"MPD",				0,            True,        True,       -1 },
	{ "Galculator",   NULL,					NULL,					0,            True,        True,       -1 },
	{ "Xmessage",     "xmessage",		NULL,					0,            True,        True,       -1 },
	{ "Nitrogen",     NULL,					NULL,					0,            True,        True,       -1 },
	{ "Wine",        "explorer.exe",NULL,					1 << 4,       False,       False,       0 },
};

//layout(s) 
static const float mfact      = 0.5;   //factor of master area size [0.05..0.95] 
static const int nmaster      = 1;     //number of clients in master area
static const Bool resizehints = False; //True means respect size hints in tiled resizals

static const Layout layouts[] = {
	//index   symbol     arrange function 
	/* 0 */ { "[T]",      tile },         //first entry is default
	/* 1 */ { "[M]",      monocle },
	/* 2 */ { "[F]",      NULL },         //no layout function means floating behavior
	/* 3 */ { "[B]",      bstack },
	/* 4 */ { "[G]",      gaplessgrid },
	/* 5 */ { "[P]",      pidgin },
};

//tagging 
static const Tag tags[] = {
	//name       layout           mfact    nmaster 
	{ "term",     &layouts[0],     -1,      -1 },
	{ "code",     &layouts[0],     -1,      -1 },
	{ "web",      &layouts[0],     -1,      -1 },
	{ "IM",       &layouts[4],     0.8,      -1 },
	{ "gaming",   &layouts[1],     -1,      -1 },
	{ "aux",      &layouts[4],     -1,      -1 },
	{ "media",    &layouts[1],     -1,      -1 },
	{ "float",    &layouts[2],     -1,      -1 },
};

//key definitions 
#define ALTKEY Mod1Mask
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

//commands 
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }
static const char terminal[]        = "urxvtc";
static const char scratchpadname[]  = "scratchy";
static const char *dmenucmd[]       = { "dmenu_run", "-i", "-p", "Run command:", "-fn", font, "-nb", colors[0][ColBG], "-nf", colors[0][ColFG],"-sb", colors[1][ColBG], "-sf", colors[1][ColFG], NULL };
static const char *termcmd[]        = { terminal, NULL };
//static const char *termscreencmd[]= { terminal, "-e", "screen", "-l", "-UDRS", "term", NULL }; 
//static const char *scratchpadcmd[]= { "urxvt", "-name", scratchpadname, "-geometry", "150x40", NULL }; 
static const char *scratchpadcmd[]  = { "emacs", "-name", scratchpadname, "-geometry", "150x40", NULL };
static const char *musiccmd[]       = { terminal, "-e", "ncmpcpp", NULL };
static const char *filemancmd[]     = { terminal, "-e", "ranger", NULL };
//static const char *browsercmd[]     = { "/bin/sh", "-c", "GTK2_RC_FILES=/usr/share/themes/Aurora/gtk-2.0/gtkrc firefox", NULL };
static const char *browsercmd[]     = { "dwb", NULL };
//static const char *altbrowsercmd[]= { "chromium", "--enable-accelerated-compositing", "--memory-model=low", NULL };
static const char *altbrowsercmd[]  = { "firefox", NULL };
static const char *imcmd[]          = { terminal, "-T", "Weechat", "-e", "screen", "-l", "-UDRS", "weechat", "weechat-curses", NULL };
static const char *emacscmd[]       = { "emacsclient", "-c", "-n", NULL };
static const char *remembercmd[]    = { "emacsclient", "--eval", "(make-remember-frame)", NULL };
static const char *mailcmd[]        = { terminal, "-T", "Mail", "-e", "mutt", NULL };
static const char *volmcmd[]        = { "amixer", "-q", "sset", "Master", "toggle", NULL };
static const char *voldcmd[]        = { "amixer", "-q", "sset", "Master", "1-", "unmute", NULL };
static const char *volucmd[]        = { "amixer", "-q", "sset", "Master", "1+", "unmute", NULL };
static const char *mixercmd[]       = { terminal, "-T", "mixer", "-e", "alsamixer", NULL };
static const char *pamixercmd[]     = { "pavucontrol", NULL };
static const char *mpdvoldcmd[]     = { "ncmpcpp", "volume", "-2", NULL };
static const char *mpdvolucmd[]     = { "ncmpcpp", "volume", "+2", NULL };
static const char *mpdtogglecmd[]   = { "ncmpcpp", "toggle", NULL };
static const char *mpdstopcmd[]     = { "ncmpcpp", "stop", NULL };
static const char *mpdprevcmd[]     = { "ncmpcpp", "prev", NULL };
static const char *mpdnextcmd[]     = { "ncmpcpp", "next", NULL };
static const char *screenshotcmd[]  = { "scrot", "-q", "100", "-e", "mv $f ~/screenshots/ 2>/dev/null", NULL };
static const char *screenoffcmd[]   = { "xset", "dpms", "force", "off", NULL };
static const char *sc2cmd[]         = { "/bin/sh", "-c", "/usr/share/playonlinux/playonlinux --run \"StarCraft II Wings of Liberty\"", NULL };
static const char *sc2gearscmd[]    = { "/home/chris/Starcraft II/Sc2gears/Sc2gears-linux.sh", NULL };

//static const char *suspendcmd[]    = { "dbus-send", "--system", "--print-reply", "--dest=org.freedesktop.UPower", "/org/freedesktop/UPower", "org.freedesktop.UPower.Suspend", NULL }; 
//static const char *hibernatecmd[]  = { "dbus-send", "--system", "--print-reply", "--dest=org.freedesktop.UPower", "/org/freedesktop/UPower", "org.freedesktop.UPower.Hibernate", NULL }; 
//static const char *rebootcmd[]     = { "dbus-send", "--system", "--print-reply", "--dest=org.freedesktop.ConsoleKit", "/org/freedesktop/ConsoleKit/Manager", "org.freedesktop.ConsoleKit.Manager.Restart", NULL }; 
//static const char *shutdowncmd[]   = { "dbus-send", "--system", "--print-reply", "--dest=org.freedesktop.ConsoleKit", "/org/freedesktop/ConsoleKit/Manager", "org.freedesktop.ConsoleKit.Manager.Stop", NULL }; 

static Key keys[] = {
	//modifier                     key         function        argument 
	{ ALTKEY,                       XK_p,       spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_Return,  spawn,          {.v = termcmd } },
	//{ MODKEY,                       XK_F1,      spawn,          {.v = termscreencmd } }, 
	{ MODKEY,                       XK_F1,       togglescratch,  {.v = scratchpadcmd } },
	{ MODKEY,                       XK_s,       spawn,          {.v = musiccmd } },
	{ MODKEY,                       XK_t,       spawn,          {.v = filemancmd } },
	{ MODKEY,                       XK_F2,      spawn,          {.v = browsercmd } },
	{ MODKEY,                       XK_F3,      spawn,          {.v = altbrowsercmd } },
	{ MODKEY,                       XK_F4,      spawn,          {.v = mailcmd } },
	{ MODKEY,                       XK_F5,      spawn,          {.v = sc2cmd } },
	{ MODKEY,                       XK_F6,      spawn,          {.v = sc2gearscmd } },
	{ MODKEY,                       XK_F12,     spawn,          {.v = screenoffcmd } },
	{ MODKEY,                       XK_e,       spawn,          {.v = emacscmd } },
	{ MODKEY,                       XK_q,       spawn,          {.v = remembercmd } },
	{ MODKEY,                       XK_g,       spawn,          {.v = imcmd } },
	{ 0,                            0x1008ff12, spawn,          {.v = volmcmd } },
	{ MODKEY,                       0x1008ff12, spawn,          {.v = mixercmd } },
	{ MODKEY|ShiftMask,             0x1008ff12, spawn,          {.v = pamixercmd } },
	{ 0,                            0x1008ff11, spawn,          {.v = voldcmd } },
	{ 0,                            0x1008ff13, spawn,          {.v = volucmd } },
	{ MODKEY,                       0x1008ff11, spawn,          {.v = mpdvoldcmd } },
	{ MODKEY,                       0x1008ff13, spawn,          {.v = mpdvolucmd } },
	{ 0,                            0x1008ff14, spawn,          {.v = mpdtogglecmd } },
	{ 0,                            0x1008ff15, spawn,          {.v = mpdstopcmd } },
	{ 0,                            0x1008ff16, spawn,          {.v = mpdprevcmd } },
	{ 0,                            0x1008ff17, spawn,          {.v = mpdnextcmd } },
	{ 0,                            XK_Print,   spawn,          {.v = screenshotcmd } },
	{ MODKEY|ShiftMask,             XK_m,       togglemax,      {0} },
	{ MODKEY|ShiftMask,             XK_b,       togglebar,      {0} },
	{ MODKEY,                       XK_j,       focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,       focusstack,     {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_j,       pushdown,       {0} },
	{ MODKEY|ShiftMask,             XK_k,       pushup,         {0} },
	{ MODKEY,                       XK_a,       incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_z,       incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,       setmfact,       {.f = -0.01} },
	{ MODKEY,                       XK_l,       setmfact,       {.f = +0.01} },
	{ MODKEY,                       XK_Return,  zoom,           {0} },
	{ MODKEY,                       XK_Tab,     view,           {0} },
	{ ALTKEY,                       XK_Tab,     focusurgent,    {0} },
	{ MODKEY,                       XK_Escape,  killclient,     {0} },
	{ MODKEY|ControlMask,           XK_F8,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY|ControlMask,           XK_F9,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY|ControlMask,           XK_F10,     setlayout,      {.v = &layouts[2]} },
	{ MODKEY|ControlMask,           XK_F11,     setlayout,      {.v = &layouts[3]} },
	{ MODKEY|ControlMask,           XK_F12,     setlayout,      {.v = &layouts[4]} },
	{ MODKEY|ControlMask|ShiftMask, XK_F12,     setlayout,      {.v = &layouts[5]} },
	{ MODKEY,                       XK_space,   setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_space,   togglefloating, {0} },
	{ MODKEY,                       XK_0,       view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,       tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,   focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period,  focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,   tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period,  tagmon,         {.i = +1 } },
	{ MODKEY,                       XK_Left,    cycle,          {.i = -1 } },
	{ MODKEY,                       XK_Right,   cycle,          {.i = +1 } },
	{ MODKEY|ControlMask,           XK_Left,    tagcycle,       {.i = -1 } },
	{ MODKEY|ControlMask,           XK_Right,   tagcycle,       {.i = +1 } },
	TAGKEYS(                        XK_1,                       0)
	TAGKEYS(                        XK_2,                       1)
	TAGKEYS(                        XK_3,                       2)
	TAGKEYS(                        XK_4,                       3)
	TAGKEYS(                        XK_5,                       4)
	TAGKEYS(                        XK_6,                       5)
	TAGKEYS(                        XK_7,                       6)
	TAGKEYS(                        XK_8,                       7)
	TAGKEYS(                        XK_9,                       8)
	//{ ALTKEY|ControlMask,           XK_s,       spawn,          {.v = suspendcmd } }, 
	//{ ALTKEY|ControlMask,           XK_h,       spawn,          {.v = hibernatecmd } }, 
	//{ ALTKEY|ControlMask,           XK_r,       spawn,          {.v = rebootcmd } }, 
	//{ ALTKEY|ControlMask,           XK_q,       spawn,          {.v = shutdowncmd } }, 
	{ MODKEY|ShiftMask,             XK_Escape,  quit,           {0} },
};

//button definitions 
//click can be ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin 
static Button buttons[] = {
	//click                event mask      button          function        argument 
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkWinTitle,          0,              Button4,        focusstack,     {.i = +1} },
	{ ClkWinTitle,          0,              Button5,        focusstack,     {.i = -1} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
	{ ClkTagBar,            0,              Button4,        cycle,          {.i = -1} },
	{ ClkTagBar,            0,              Button5,        cycle,          {.i = +1} },
};
