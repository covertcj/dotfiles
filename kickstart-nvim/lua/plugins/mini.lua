-- Collection of various small independent plugins/modules
return {
  'echasnovski/mini.nvim',
  config = function()
    -- Better Around/Inside textobjects
    --
    -- Examples:
    --  - va)  - Visually select Around )paren
    --  - yinq - Yank Inside Next 'quote
    --  - ci'  - Change Inside 'quote
    require('mini.ai').setup { n_lines = 500 }

    -- Add/delete/replace surroundings (brackets, quotes, etc.)
    --
    -- - saiw) - Surround Add Inner Word )Paren
    -- - sd'   - Surround Delete 'quotes
    -- - sr)'  - Surround Replace ) '
    require('mini.surround').setup()

    -- ... and there is more!
    --  Check out: https://github.com/echasnovski/mini.nvim
  end,
}
