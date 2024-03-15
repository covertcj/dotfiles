return {
  {
    'bluz71/vim-moonfly-colors',
    priority = 1000,
    init = function()
      vim.cmd.colorscheme 'moonfly'
    end,
  },
}
