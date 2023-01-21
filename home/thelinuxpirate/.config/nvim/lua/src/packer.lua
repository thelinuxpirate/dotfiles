-- Only required if you have packer configured as `opt`
  vim.cmd [[packadd packer.nvim]]

  return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'
    use 'Mofiqul/dracula.nvim'
    use 'habamax/vim-godot'

    use {
	  'nvim-treesitter/nvim-treesitter',
	  run = ':TSUpdate'
    }

    use {'nvim-orgmode/orgmode', config = function()
    require('orgmode').setup{} }

end)
