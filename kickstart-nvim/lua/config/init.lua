vim.g.cjc_colorscheme = 'material'

require 'config.options'
require 'config.keys'
require 'config.lazy'

vim.g.material_style = 'deep ocean'
vim.cmd.colorscheme 'material'

-- Highlight when yanking (copying) text, try it with `yap` in normal mode
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})
