-- SRC
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.opt.termguicolors = true

vim.cmd.colorscheme "catppuccin-mocha"
vim.o.number = true
vim.o.cursorline = true

vim.o.tabstop = 4
vim.o.shiftwidth = 2
vim.o.expandtab = true
vim.o.smarttab = true

vim.opt.spelllang = 'en'

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
