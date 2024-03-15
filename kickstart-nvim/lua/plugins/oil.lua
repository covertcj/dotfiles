-- fancy file browsing
return {
  'stevearc/oil.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons' },

  config = function()
    require('oil').setup {
      delete_to_trash = true,
      view_options = {
        show_hidden = true,
      },
    }

    vim.keymap.set('n', '<leader>ff', '<cmd>Oil<cr>', { desc = '[F]ile explorer' })
  end,
}
