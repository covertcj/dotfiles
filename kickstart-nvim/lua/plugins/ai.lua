-- TODO: investigate using inline copilot suggestions
-- here's a good reference implementation of working with cmp
-- https://github.com/fredrikaverpil/dotfiles/blob/main/nvim-lazyvim/lua/plugins/ai.lua
return {
  {
    'zbirenbaum/copilot.lua',
    opts = {
      suggestion = { enabled = false },
      panel = { enabled = false },
    },
  },
  {
    'zbirenbaum/copilot-cmp',
    opts = {},
  },
  {
    'CopilotC-Nvim/CopilotChat.nvim',
    keys = {
      { '<leader>oc', '<cmd>CopilotChatToggle<cr>', desc = 'Open Copilot Chat' },
      { '<leader>oe', '<cmd>CopilotChatExplain<cr>', desc = 'Open Copilot Explain' },
    },
    branch = 'canary',
    dependencies = {
      { 'zbirenbaum/copilot.lua' }, -- or github/copilot.vim
      { 'nvim-lua/plenary.nvim' }, -- for curl, log wrapper
    },
    opts = {},
  },
  },
}
