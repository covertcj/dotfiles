return {
  'nvim-lualine/lualine.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  opts = {
    sections = {
      lualine_a = {
        {
          'mode',
          fmt = function(str)
            return str:sub(1, 1)
          end,
        },
      },
      lualine_b = { 'diff', 'diagnostics' },
      lualine_c = {
        { 'filename', path = 1 },
      },
      lualine_x = {
        { 'filetype', icon_only = true },
      },
    },
  },
}
