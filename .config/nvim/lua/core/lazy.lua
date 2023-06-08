local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

local plugins = { 
  {
  "startup-nvim/startup.nvim",
  dependencies = { {"nvim-telescope/telescope.nvim"}, {"nvim-lua/plenary.nvim"} },
  },

  { 
  'catppuccin/nvim', 
    lazy = true,
    dependencies = { {'sharkdp/fd'}, {'nvim-tree/nvim-web-devicons', lazy = true} }
  },

  {
  "folke/which-key.nvim",
  event = "VeryLazy",
  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
  end,
},

  'nvim-treesitter/nvim-treesitter',
  
  {
  'nvim-telescope/telescope.nvim',
    dependencies = { {'nvim-lua/plenary.nvim'} }
  },

  'andweeb/presence.nvim',
 
  { 
  'nvim-tree/nvim-tree.lua',
    dependencies = { 'nvim-tree/nvim-web-devicons' } 
  },

}

require("lazy").setup(plugins, opts)
