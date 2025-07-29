local wezterm = require 'wezterm'

return {
    enable_wayland = false,
    front_end = 'OpenGL',

    -- appearence
    color_scheme = 'Catppuccin Mocha',
    font = wezterm.font('FiraCode'),
    font_size = 11.5,

    cell_width = 1,
    line_height = 1.3,

    bold_brightens_ansi_colors = true,
    window_decorations = 'RESIZE',
    hide_tab_bar_if_only_one_tab = true,

    -- terminal features
    enable_kitty_keyboard = true,
    hide_mouse_cursor_when_typing = true,
}
