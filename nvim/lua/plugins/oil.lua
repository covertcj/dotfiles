-- fancy file browsing
return {
  'stevearc/oil.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  opts = {
    delete_to_trash = true,
    view_options = {
      show_hidden = true,
    },
    win_options = {
      winbar = "%{v:lua.require('oil').get_current_dir()}",
    },
    -- the default keymaps bind C-h and C-l which I use to switch windows
    use_default_keymaps = false,
    keymaps = {
      ['g?'] = 'actions.show_help',
      ['<CR>'] = 'actions.select',
      ['<C-p>'] = 'actions.preview',
      ['<C-c>'] = 'actions.close',
      ['<C-r>'] = 'actions.refresh',
      ['-'] = 'actions.parent',
      ['_'] = 'actions.open_cwd',
      ['`'] = 'actions.cd',
      ['~'] = { 'actions.cd', opts = { scope = 'tab' }, desc = ':tcd to the current oil directory' },
      ['gs'] = 'actions.change_sort',
      ['gx'] = 'actions.open_external',
      ['g.'] = 'actions.toggle_hidden',
      ['g\\'] = 'actions.toggle_trash',
    },
  },
  keys = {
    { '<leader>ff', '<cmd>Oil<cr>', desc = 'File explorer' },
  },
}
