return {
  'folke/which-key.nvim',
  event = 'VimEnter',
  config = function()
    require('which-key').setup()

    -- document existing key chains
    require('which-key').register {
      ['<leader>b'] = { name = 'Buffer', _ = 'which_key_ignore' },
      ['<leader>c'] = { name = 'Code', _ = 'which_key_ignore' },
      ['<leader>f'] = { name = 'Find', _ = 'which_key_ignore' },
      ['<leader>g'] = { name = 'Git', _ = 'which_key_ignore' },
      ['<leader>t'] = { name = 'Setting toggles', _ = 'which_key_ignore' },
    }
  end,
}
