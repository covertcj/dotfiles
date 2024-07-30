return {
  'ThePrimeagen/harpoon',
  branch = 'harpoon2',
  dependencies = { 'nvim-lua/plenary.nvim' },
  keys = {
    {
      '<leader>ha',
      function()
        require('harpoon'):list():add()
      end,
      desc = 'Add file',
    },
    {
      '<leader>hl',
      function()
        local harpoon = require 'harpoon'
        harpoon.ui:toggle_quick_menu(harpoon:list())
      end,
      desc = 'List',
    },
    {
      '<M-!>',
      function()
        require('harpoon'):list():select(1)
      end,
      desc = 'Harpoon 1',
    },
    {
      '<M-@>',
      function()
        require('harpoon'):list():select(2)
      end,
      desc = 'Harpoon 2',
    },
    {
      '<M-#>',
      function()
        require('harpoon'):list():select(3)
      end,
      desc = 'Harpoon 3',
    },
    {
      '<M-$>',
      function()
        require('harpoon'):list():select(4)
      end,
      desc = 'Harpoon 4',
    },
    {
      '<M-J>',
      function()
        require('harpoon'):list():next()
      end,
      desc = 'Harpoon Next',
    },
    {
      '<M-K>',
      function()
        require('harpoon'):list():prev()
      end,
      desc = 'Harpoon Previous',
    },
  },
  opts = {},
}
