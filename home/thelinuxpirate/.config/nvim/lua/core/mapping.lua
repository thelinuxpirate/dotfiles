-- Leader Keys
vim.g.maplocalleader = ";"
vim.g.mapleader = " "

-- File Management
vim.keymap.set("n", "<leader>f", "<cmd>Telescope find_files<CR>",
{ desc = "Find Files using Telescope" })
  
-- NetRW
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)
-- Buffers
vim.keymap.set("n", "<leader>bd", vim.cmd.bdelete)
vim.keymap.set("n", "<leader>bk", vim.cmd.Bclose)

-- Splits
vim.keymap.set("n", "<leader>bss", vim.cmd.vsplit)
vim.keymap.set("n", "<leader>bsh", vim.cmd.split)

vim.keymap.set("n", "<localleader>jf",  "<cmd>Telescope buffers<CR>", 
{ desc = "List Buffers & Switch Buffers (Telescope)" })

-- Misc
vim.keymap.set("n", "<localleader>s", vim.cmd.so)

vim.keymap.set("n", "<leader>t",  vim.cmd.terminal)
vim.keymap.set("n", "<leader>ll", vim.cmd.Lazy)
