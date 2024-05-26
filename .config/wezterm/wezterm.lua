local wezterm = require 'wezterm'
return {
	enable_wayland = false, -- currently broken?

	-- appearence
  color_scheme = 'Tokyo Night',
	font = wezterm.font 'Comic Mono',
	front_end = 'OpenGL',

	window_decorations = 'RESIZE',
	line_height = 0.99,
	enable_tab_bar = false,

	-- terminal features
	enable_kitty_keyboard = true,
	hide_mouse_cursor_when_typing = true,
}
