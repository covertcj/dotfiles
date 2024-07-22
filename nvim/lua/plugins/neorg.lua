return {
  {
    'nvim-neorg/neorg',
    dependencies = { 'vhyrro/luarocks.nvim' },
    opts = {
      load = {
        ['core.defaults'] = {},
        ['core.completion'] = {
          config = {
            engine = 'nvim-cmp',
            name = '[Norg]',
          },
        },
        ['core.integrations.nvim-cmp'] = {},
        ['core.concealer'] = {},
        ['core.keybinds'] = {
          config = {
            default_keybinds = true,
            neorg_leader = '<leader>',
          },
        },
        ['core.dirman'] = {
          config = {
            workspaces = {
              Notes = '~/neorg/notes',
            },
          },
        },
      },
    },
  },
}
