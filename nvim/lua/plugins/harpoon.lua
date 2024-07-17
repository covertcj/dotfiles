return {
  'ThePrimeagen/harpoon',
  branch = 'harpoon2',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    local harpoon = require('harpoon').setup()

    vim.keymap.set('n', '<leader>ha', function()
      harpoon:list():append()
    end, { desc = 'Add file' })
    vim.keymap.set('n', '<leader>hl', function()
      harpoon.ui:toggle_quick_menu(harpoon:list())
    end, { desc = 'List' })

    for i = 1, 10, 1 do
      vim.keymap.set('n', '<leader>h' .. i, function()
        harpoon:list():select(i)
      end, { desc = 'Harpoon ' .. i })
    end
  end,
}
