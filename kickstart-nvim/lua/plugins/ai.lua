-- TODO: investigate using inline copilot suggestions
-- here's a good reference implementation of working with cmp
-- https://github.com/fredrikaverpil/dotfiles/blob/main/nvim-lazyvim/lua/plugins/ai.lua
return {
  {
    'zbirenbaum/copilot.lua',
    config = function()
      require('copilot').setup {
        suggestion = { enabled = false },
        panel = { enabled = false },
      }
    end,
  },
  {
    'zbirenbaum/copilot-cmp',
    config = function()
      require('copilot_cmp').setup()
    end,
  },
}
