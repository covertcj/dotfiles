return {
  {
    'catppuccin/nvim',
    name = 'catppuccin',
    priority = 1000,
    keys = {
      { '<leader>tt', '<CMD>colorscheme catppuccin-mocha<CR>', desc = 'Dark theme' },
      { '<leader>tT', '<CMD>colorscheme catppuccin-latte<CR>', desc = 'Light theme' },
    },
    lazy = false,
    config = function()
      vim.cmd.colorscheme 'catppuccin-mocha'
    end,
  },
}
