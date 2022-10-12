-- Variables
local opt = vim.opt
local cmd = vim.cmd
local o = vim.o

--[[ For Gruvbox Theme:
o.background = "dark" -- or "light" for light mode
require("gruvbox").setup({
undercurl = true,
underline = true,
bold = true,
italic = true,
strikethrough = true,
invert_selection = false,
invert_signs = false,
invert_tabline = false,
invert_intend_guides = false,
inverse = true, -- invert background for search, diffs, statuslines and errors
contrast = "", -- can be "hard", "soft" or empty string
o verrides = {},
} )
cmd[[colorscheme gruvbox]] --]]

--[[ For Tokyo Night:
cmd[[colorscheme tokyonight]] --]]

-- Nightfox Colorschemes:

cmd("colorscheme carbonfox") -- Options are: nightfox, dayfox, dawnfox, duskfox, nordfox, terafox, carbonfox
