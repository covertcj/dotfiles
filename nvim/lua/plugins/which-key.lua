return {
  'folke/which-key.nvim',
  event = 'VeryLazy',
  config = function()
    local wk = require 'which-key'
    wk.setup()

    -- document existing key chains
    wk.add {
      { '<leader>b', group = 'Buffer' },
      { '<leader>c', group = 'Code' },
      { '<leader>f', group = 'Find' },
      { '<leader>g', group = 'Git' },
      { '<leader>h', group = 'Harpoon' },
      { '<leader>o', group = 'Open' },
      { '<leader>t', group = 'Setting toggles' },
    }
  end,
}
