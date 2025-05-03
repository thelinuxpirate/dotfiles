local wezterm = require 'wezterm'

return {
    enable_wayland = false,
    front_end = 'OpenGL',

    -- appearence
    color_scheme = 'Tokyo Night',
    font = wezterm.font('Comic Mono', { weight = 'Bold', italic = false }),
    font_size = 12.12,

    cell_width = 1,
    line_height = 1.2,

    bold_brightens_ansi_colors = true,
    window_decorations = 'RESIZE',
    line_height = 1.025,
    hide_tab_bar_if_only_one_tab = true,

    -- terminal features
    enable_kitty_keyboard = true,
    hide_mouse_cursor_when_typing = true,
}
