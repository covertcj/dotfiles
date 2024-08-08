return {
  'folke/trouble.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    -- TODO: possibly use Trouble for LSP commands like finding references and definitions
    -- TODO investigate telescope.nvim integration
    vim.keymap.set('n', '<leader>od', '<cmd>TroubleToggle<cr>', { desc = 'Diagnostics' })
  end,
}
