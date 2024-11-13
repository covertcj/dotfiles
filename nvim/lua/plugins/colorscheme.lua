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
      require('catppuccin').setup {
        flavor = 'auto',
        background = {
          light = 'latte',
          dark = 'mocha',
        },
        no_italic = true,
        color_overrides = {
          mocha = {
            base = '#201d18',
            crust = '#1b1111',
            mantle = '#1e1910',
            surface0 = '#302a20',
          },
        },
      }

      vim.cmd.colorscheme 'catppuccin'
    end,
  },
}
