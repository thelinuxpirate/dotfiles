-- I personally use Packer as the AUR package 
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
	-- Themes
	use { 'catppuccin/nvim', as = 'catppuccin', {'sharkdp/fd'}, {'nvim-tree/nvim-web-devicons'} }

	-- Tree-Sitter
	use { 'nvim-treesitter/nvim-treesitter',
	      run = function()
            	local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
            	ts_update()
        end,}

	-- Telescope
	use { 'nvim-telescope/telescope.nvim', {'nvim-lua/plenary.nvim'} }    	

	-- ThePrimeageon Harpoon & Dependencies 
	--use { 'nvim-lua/plenary.nvim', {'ThePrimeagen/harpoon', opt = true} }
end)
