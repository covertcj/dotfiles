return {
  {
    'lewis6991/gitsigns.nvim',
    opts = {
      signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      },
    },
  },

  {
    'tpope/vim-fugitive',
    keys = {
      { '<leader>gG', '<cmd>vertical rightbelow Git<cr>', desc = 'Git status' },
      { '<leader>gp', '<cmd>Git pull<cr>', desc = 'Git pull' },
      { '<leader>gP', '<cmd>Git push<cr>', desc = 'Git push' },
      { '<leader>gf', '<cmd>Git fetch<cr>', desc = 'Git fetch' },
    },
    config = function()
      vim.api.nvim_create_augroup('FugitiveKeys', { clear = true })
      vim.api.nvim_create_autocmd('Filetype', {
        group = 'FugitiveKeys',
        pattern = { 'fugitive' },
        callback = function()
          vim.keymap.set('n', '<TAB>', '=', { buffer = 0, remap = true })
        end,
      })
    end,
  },
  {
    'f-person/git-blame.nvim',
  },
  {
    'NeogitOrg/neogit',
    dependencies = {
      'nvim-lua/plenary.nvim', -- required
      'sindrets/diffview.nvim', -- optional - Diff integration

      -- Only one of these is needed, not both.
      'nvim-telescope/telescope.nvim', -- optional
      --'ibhagwan/fzf-lua', -- optional
    },
    keys = { { '<leader>gg', '<cmd>Neogit<cr>', desc = 'Git status' } },
    opts = {
      disable_context_highlighting = true,
      graph_style = 'unicode',
      integrations = {
        telescope = true,
        diffview = true,
      },
    },
  },
}
