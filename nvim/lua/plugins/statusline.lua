return {
  'nvim-lualine/lualine.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  opts = {
    sections = {
      lualine_b = { 'diff', 'diagnostics' },
      lualine_c = {
        {
          'filename',
          path = 1,
        },
      },
    },
  },
}
