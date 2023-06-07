-- Leader Keys
vim.g.maplocalleader = " "
vim.g.mapleader = ";"

-- File Management
vim.keymap.set("n", "<leader>f", "<cmd>Telescope find_files<cr>",
{ desc = "Find Files using Telescope" })
  
-- Buffers
vim.keymap.set("n", "<leader>jf",  "<cmd>Telescope buffers inital_mode=normal<cr>", 
{ desc = "List Buffers & Switch Buffers (Telescope)" })

vim.keymap.set("n", "<leader>k", "<cmd>bd #<cr>",
{ desc = "Delete current buffer & window" })

-- Window Management
  -- Split Windows
  vim.keymap.set("n", "<leader>ss", vim.cmd.vsplit, 
  { desc = "Split window vertically" })
  vim.keymap.set("n", "<leader>sh", vim.cmd.split,
  { desc = "Split window horizontally" })
 
  -- Switch Window Focus
  vim.keymap.set("n", "<localleader>h", "<C-W>h",
  { desc = "Switch window focus left" })
  vim.keymap.set("n", "<localleader>j", "<C-W>j",
  { desc = "Switch window focus down" })
  vim.keymap.set("n", "<localleader>k", "<C-W>k",
  { desc = "Switch window focus up" })
  vim.keymap.set("n", "<localleader>l", "<C-W>l",
  { desc = "Switch window focus right" })

  -- Switch Window Placement
  vim.keymap.set("n", "<localleader>u", "<C-W>H",
  { desc = "Move current window left" })
  vim.keymap.set("n", "<localleader>i", "<C-W>J",
  { desc = "Move current window down" })
  vim.keymap.set("n", "<localleader>o", "<C-W>K",
  { desc = "Move current window up" })
  vim.keymap.set("n", "<localleader>p", "<C-W>L",
  { desc = "Move current window right" })

  vim.keymap.set("n", "<localleader>r", "<C-W>r",
  { desc = "Rotate windows downwards/rightwards" })
  vim.keymap.set("n", "<localleader>s", "<C-W>x", 
  { desc = "Exchange current window with next one" })

-- Misc
vim.keymap.set("n", "<leader>s", vim.cmd.so,
{ desc = "Source current file" })

vim.keymap.set("n", "<leader>r", vim.cmd.redo,
{ desc = "Redo last command" })

vim.keymap.set("n", "<leader>t",  vim.cmd.terminal, 
{ desc = "Open a terminal in current session" })
vim.keymap.set("n", "<localleader>ll", vim.cmd.Lazy, 
{ desc = "Open Lazy plugin manager" })
