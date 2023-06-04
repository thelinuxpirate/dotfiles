require('tlp')

-- Variables
local cmd = vim.cmd
local opt = vim.opt
local g = vim.g
local keymap = vim.keymap
local wo = vim.wo

-- SRC
cmd.colorscheme "catppuccin-mocha"
wo.number = true
-- Keybinds
g.mapleader = " "
keymap.set("n", "<leader>f", cmd.Telescope)
keymap.set("n", "<leader>pv", cmd.Ex)

-- Neovide
if vim.g.neovide then
	vim.o.guifont = "JetBrainsMono Nerd Font:h14"
	-- Helper function for transparency formatting
	local alpha = function()
  	  return string.format("%x", math.floor(255 * vim.g.transparency or 0.8))
	end
	-- g:neovide_transparency should be 0 if you want to unify transparency of content and title bar.
	vim.g.neovide_transparency = 0.8
	vim.g.transparency = 0.8
	vim.g.neovide_background_color = "#0f1117" .. alpha()
	vim.g.neovide_floating_blur_amount_x = 2.0
	vim.g.neovide_floating_blur_amount_y = 2.0
end

