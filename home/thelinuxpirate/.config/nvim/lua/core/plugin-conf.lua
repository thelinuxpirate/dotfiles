-- Telescope
  local telescope = require('telescope')
  telescope.setup {
    pickers = {
      find_files = {
        hidden = true
      }
    }
  }

-- Nvim-Tree
require("nvim-tree").setup({
  sort_by = "case_sensitive",
  view = {
    width = 30,
  },
  renderer = {
    group_empty = true,
  },
  filters = {
    dotfiles = true,
  },
})
