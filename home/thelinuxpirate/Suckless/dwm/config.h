#include <X11/XF86keysym.h>

static const unsigned int borderpx       = 2;   /* border pixel of windows */
static const unsigned int snap           = 32;  /* snap pixel */
static const int showbar                 = 1;   /* 0 means no bar */
static const int topbar                  = 1;   /* 0 means bottom bar */
static const int bar_height              = 0;   /* 0 means derive from font, >= 1 explicit height */
static const int focusonwheel            = 0;
static const int statusmon               = 'A';
static const int horizpadbar             = 2;   /* horizontal padding for statusbar */
static const int vertpadbar              = 0;   /* vertical padding for statusbar */

static int tagindicatortype              = INDICATOR_TOP_LEFT_SQUARE;
static int tiledindicatortype            = INDICATOR_NONE;
static int floatindicatortype            = INDICATOR_TOP_LEFT_SQUARE;
static const char *fonts[]               = { "monospace:size=10",
  "Font Awesome 6 Free Solid:size=12", "Font Awesome 6 Brands:size=12" };
static const char dmenufont[]            = "monospace:size=10";

static char c000000[]                    = "#000000"; // placeholder value

static char normfgcolor[]                = "#50fa7b"; // Basic Font Color for the Bar;
static char normbgcolor[]                = "#282a36"; // Bar color;
static char normbordercolor[]            = "#282a36"; // Basic Border color for the Bar;
static char normfloatcolor[]             = "#ffb86c"; // Floating Window Border Color (?);

static char selfgcolor[]                 = "#44475a"; // Dmenu Text Color WHEN HIGHLIGHTED;
static char selbgcolor[]                 = "#6272a4"; // Dmenu Highlight Text Color;

static char selbordercolor[]             = "#ffb86c"; // Window Border Color;
static char selfloatcolor[]              = "#ff79c6"; // Window Floating Color;

static char titlenormfgcolor[]           = "#50fa7b"; // POSSIBLE WINDOW TITLES (?);
static char titlenormbgcolor[]           = "#50fa7b"; // (?);
static char titlenormbordercolor[]       = "#50fa7b"; // (?);
static char titlenormfloatcolor[]        = "#50fa7b"; // (?);

static char titleselfgcolor[]            = "#50fa7b"; // (?);
static char titleselbgcolor[]            = "#ff5555"; // (?);
static char titleselbordercolor[]        = "#ff5555"; // (?);
static char titleselfloatcolor[]         = "#ff5555"; // (?);

static char tagsnormfgcolor[]            = "#53BDBD"; // Inactive Tag Text Color;
static char tagsnormbgcolor[]            = "#282a36"; // Inactive Tag Box Color;
static char tagsnormbordercolor[]        = "#6272a4"; // (?);
static char tagsnormfloatcolor[]         = "#ff5555"; // (?);

static char tagsselfgcolor[]             = "#50fa7b"; // Active Tag Text Color;
static char tagsselbgcolor[]             = "#44475a"; // Active Tag Box Color;
static char tagsselbordercolor[]         = "#005577"; // (?);
static char tagsselfloatcolor[]          = "#005577"; // (?);

static char hidnormfgcolor[]             = "#005577"; // (?);
static char hidselfgcolor[]              = "#227799"; // (?);
static char hidnormbgcolor[]             = "#222222"; // (?);
static char hidselbgcolor[]              = "#222222"; // (?);

static char urgfgcolor[]                 = "#bbbbbb"; // URGENT COLORS
static char urgbgcolor[]                 = "#222222";
static char urgbordercolor[]             = "#ff0000";
static char urgfloatcolor[]              = "#db8fd9";

static char *colors[][ColCount] = {
	[SchemeNorm]         = { normfgcolor,      normbgcolor,      normbordercolor,      normfloatcolor },
	[SchemeSel]          = { selfgcolor,       selbgcolor,       selbordercolor,       selfloatcolor },
	[SchemeTitleNorm]    = { titlenormfgcolor, titlenormbgcolor, titlenormbordercolor, titlenormfloatcolor },
	[SchemeTitleSel]     = { titleselfgcolor,  titleselbgcolor,  titleselbordercolor,  titleselfloatcolor },
	[SchemeTagsNorm]     = { tagsnormfgcolor,  tagsnormbgcolor,  tagsnormbordercolor,  tagsnormfloatcolor },
	[SchemeTagsSel]      = { tagsselfgcolor,   tagsselbgcolor,   tagsselbordercolor,   tagsselfloatcolor },
	[SchemeHidNorm]      = { hidnormfgcolor,   hidnormbgcolor,   c000000,              c000000 },
	[SchemeHidSel]       = { hidselfgcolor,    hidselbgcolor,    c000000,              c000000 },
	[SchemeUrg]          = { urgfgcolor,       urgbgcolor,       urgbordercolor,       urgfloatcolor },
};

static char *tagicons[][NUMTAGS] = {
	[DEFAULT_TAGS]        = { "emac", "web", "music", "discrd",
				  "stem", "gd", "othr", "ctrl" },
	[ALTERNATIVE_TAGS]    = { "1", "2", "3", "4", "5", "6", "7", "8" },
	[ALT_TAGS_DECORATION] = { "<1>", "<2>", "<3>", "<4>", "<5>", "<6>", "<7>" },
};

static const BarRule barrules[] = {
	{ -1,        0,     BAR_ALIGN_LEFT,   width_tags,               draw_tags,              click_tags,              hover_tags,              "tags" },
	{ -1,        0,     BAR_ALIGN_LEFT,   width_ltsymbol,           draw_ltsymbol,          click_ltsymbol,          NULL,                    "layout" },
	{ statusmon, 0,     BAR_ALIGN_RIGHT,  width_status2d,           draw_status2d,          click_status2d,          NULL,                    "status2d" },
};

static const Rule rules[] = {
	RULE(.wintype = WTYPE "DIALOG", .isfloating = 1)
	RULE(.wintype = WTYPE "UTILITY", .isfloating = 1)
	RULE(.wintype = WTYPE "TOOLBAR", .isfloating = 1)
	RULE(.wintype = WTYPE "SPLASH", .isfloating = 1)
	RULE(.class = "Gimp", .tags = 1 << 4)
	RULE(.class = "Firefox", .tags = 1 << 7)
};

static const float mfact     = 0.55; 
static const int nmaster     = 1;   
static const int resizehints = 0;  
static const int lockfullscreen = 1;

static const Layout layouts[] = {
	{ "|M|",      centeredmaster }, // 0
	{ "TTT",      bstack }, // 1
	{ "[]=",      tile }, // 2
	{ "[\\]",     dwindle }, // 3
	{ "(@)",      spiral }, // 4
	{ "><><",     NULL }, // 5
	{ "[F]",      monocle }, // 6
	{ NULL,       NULL },

};

#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

static char dmenumon[2] = "0";
static const char *dmenucmd[] = {
	"dmenu_run",
	"-m", dmenumon,
	"-fn", dmenufont,
	"-nb", normbgcolor,
	"-nf", normfgcolor,
	"-sb", selbgcolor,
	"-sf", selfgcolor,
	topbar ? NULL : "-b",
	NULL
};

static const char *termcmd[]    = { "alacritty", NULL };
static const char *webcmd[]     = { "brave-browser", NULL };
static const char *editorcmd[]  = { "emacs", NULL };
static const char *discordcmd[] = { "discord", NULL };
static const char *playercmd[]  = { "spotify", NULL };


static const Key keys[] = {

	// Commands & Variables
	{ MODKEY,                       XK_space,                 spawn,                  {.v = dmenucmd } },
	{ MODKEY,                       XK_Return,                spawn,                  {.v = termcmd } },
	{ MODKEY, 	   	   	  XK_e,	                    spawn,     	            {.v = editorcmd } },
	{ MODKEY|ShiftMask, 		  XK_d,	                    spawn, 	            {.v = discordcmd} },
	{ MODKEY|ShiftMask, 		  XK_s,	                    spawn, 	            {.v = playercmd } },
	{ MODKEY|ShiftMask,             XK_b,                     spawn,                  {.v = webcmd } },
	{ MODKEY|ShiftMask,             XK_l,                     spawn,                  SHCMD("slock") },

	// Window Management
	{ MODKEY,                       XK_b,                     togglebar,              {0} },
	{ MODKEY,                       XK_j,                     focusstack,             {.i = +1 } },
	{ MODKEY,                       XK_k,                     focusstack,             {.i = -1 } },
	{ MODKEY,                       XK_o,                     incnmaster,             {.i = +1 } },
	{ MODKEY,                       XK_d,                     incnmaster,             {.i = -1 } },
	{ MODKEY,                       XK_h,                     setmfact,               {.f = -0.05} },
	{ MODKEY,                       XK_l,                     setmfact,               {.f = +0.05} },
	{ MODKEY|ShiftMask,             XK_Return,                zoom,                   {0} },
	{ MODKEY,                       XK_Tab,                   view,                   {0} },
	{ MODKEY,                       XK_w,                     killclient,             {0} },
	{ MODKEY|ShiftMask,             XK_q,                     quit,                   {0} },

	// Layouts
	{ MODKEY,                       XK_t,                     setlayout,              {.v = &layouts[2]} },
	{ MODKEY,                       XK_f,                     setlayout,              {.v = &layouts[3]} },
	{ MODKEY,                       XK_i,                     setlayout,              {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,                     setlayout,              {.v = &layouts[0]} },
	{ MODKEY|ShiftMask,             XK_space,                 togglefloating,         {0} },
	{ MODKEY|ShiftMask,             XK_f,                     fullscreen,             {0} },
	{ MODKEY|ShiftMask,             XK_p,                     cyclelayout,            {.i = -1 } },
	{ MODKEY,                       XK_p,                     cyclelayout,            {.i = +1 } },

	// XF86 Keys
	{ MODKEY,                       XF86XK_AudioRaiseVolume,  spawn,                  SHCMD("pamixer -i 2") },
	{ MODKEY,                       XF86XK_AudioLowerVolume,  spawn,                  SHCMD("pamixer -d 2") },
	{ MODKEY,                       XF86XK_AudioMute,         spawn,                  SHCMD("pamixer -t") },
	{ MODKEY,                       XF86XK_MonBrightnessUp,   spawn,                  SHCMD("xbacklight -inc 5") },
	{ MODKEY,                       XF86XK_MonBrightnessDown, spawn,                  SHCMD("xbacklight -dec 5") },

	// Tags
	{ MODKEY,                       XK_0,          view,                   {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,          tag,                    {.ui = ~0 } },
	{ MODKEY,                       XK_comma,      focusmon,               {.i = -1 } },
	{ MODKEY,                       XK_period,     focusmon,               {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,      tagmon,                 {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period,     tagmon,                 {.i = +1 } },

	TAGKEYS(                        XK_1,                                  0)
	TAGKEYS(                        XK_2,                                  1)
	TAGKEYS(                        XK_3,                                  2)
	TAGKEYS(                        XK_4,                                  3)
	TAGKEYS(                        XK_5,                                  4)
	TAGKEYS(                        XK_6,                                  5)
	TAGKEYS(                        XK_7,                                  6)
	TAGKEYS(                        XK_8,                                  7)
	TAGKEYS(                        XK_9,                                  8)
};

static const Button buttons[] = {
	{ ClkLtSymbol,          0,                   Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,                   Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,                   Button2,        zoom,           {0} },
	{ ClkStatusText,        0,                   Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,              Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,              Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,              Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,                   Button1,        view,           {0} },
	{ ClkTagBar,            0,                   Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,              Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,              Button3,        toggletag,      {0} },
};
