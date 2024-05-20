return {
  {
    'marko-cerovac/material.nvim',
    keys = {
      { '<leader>tt', '<cmd>lua require("material.functions").change_style("deep ocean")<cr>', desc = 'Dark theme' },
      { '<leader>tT', '<cmd>lua require("material.functions").change_style("lighter")<cr>', desc = 'Light theme' },
    },
    lazy = false,
    config = function()
      vim.g.material_style = 'darker'

      require('material').setup {
        contrast = {
          non_current_windows = true,
        },
        plugins = {
          -- "dap",
          -- "dashboard",
          -- "eyeliner",
          -- "fidget",
          -- "flash",
          'gitsigns',
          'harpoon',
          -- "hop",
          -- "illuminate",
          -- "indent-blankline",
          'lspsaga',
          'mini',
          'neogit',
          'neotest',
          -- "neo-tree",
          'neorg',
          -- "noice",
          'nvim-cmp',
          -- "nvim-navic",
          -- "nvim-tree",
          'nvim-web-devicons',
          -- "rainbow-delimiters",
          -- "sneak",
          'telescope',
          'trouble',
          'which-key',
          -- "nvim-notify",
        },

        high_visibility = {
          lighter = true,
        },
      }

      vim.cmd.colorscheme 'material'
    end,
  },
}
