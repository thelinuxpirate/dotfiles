const unsigned int interval = 1000;

static const char unknown_str[] = "N/A";

#define MAXLEN 2048

static const struct arg args[] = {
	{ uptime,       "^c#6FBD42^ [UTIME %s] ",                     NULL },
	{ run_command,  "^c#53BDBD^ [VOL %s%] ^c#EBCC5E^ | ",         "pamixer --get-volume" },
	{ ram_perc,     "^c#BA1417^ [RAM %s%] ",                      NULL },
	{ cpu_perc,     "^c#EAF520^ [CPU %s%] ",                      NULL },
	{ battery_perc, "^c#6FBD42^ [BAT %s%] ^c#EBCC5E^ | ",  	      "BAT0" },
	{ datetime,     "^c#F0A10E^ [DATE %s]",                       "%a %b %d %r" },
};
