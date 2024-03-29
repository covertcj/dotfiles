local function filenameFirst(_, path)
  local tail = vim.fs.basename(path)
  local parent = vim.fs.dirname(path)
  if parent == '.' then
    return tail
  end
  return string.format('%s\t\t%s', tail, parent)
end

-- recolors telescope results to work with the filenameFirst path_display
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'TelescopeResults',
  callback = function(ctx)
    vim.api.nvim_buf_call(ctx.buf, function()
      vim.fn.matchadd('TelescopeParent', '\t\t.*$')
      vim.api.nvim_set_hl(0, 'TelescopeParent', { link = 'Comment' })
    end)
  end,
})

return {
  'nvim-telescope/telescope.nvim',
  event = 'VimEnter',
  branch = '0.1.x',
  dependencies = {
    'nvim-lua/plenary.nvim',
    {
      'nvim-telescope/telescope-fzf-native.nvim',
      build = 'make',
      cond = function()
        return vim.fn.executable 'make' == 1
      end,
    },
    { 'nvim-telescope/telescope-ui-select.nvim' },
    { 'nvim-tree/nvim-web-devicons', enabled = vim.g.have_nerd_font },
  },
  keys = {
    { '<leader>fh', '<cmd>Telescope help_tags<cr>', desc = 'Help' },
    { '<leader>fk', '<cmd>Telescope keymaps<cr>', desc = 'Keymaps' },
    { '<leader><leader>', '<cmd>Telescope find_files<cr>', desc = 'Files' },
    { '<C-p>', '<cmd>Telescope find_files<cr>', desc = 'Files' },
    { '<leader>ft', '<cmd>Telescope builtin<cr>', desc = 'Telescope commands' },
    { '<leader>fw', '<cmd>Telescope grep_string<cr>', desc = 'Word under cursor' },
    { '<leader>fg', '<cmd>Telescope live_grep<cr>', desc = 'Grep' },
    { '<leader>fd', '<cmd>Telescope diagnostics<cr>', desc = 'Diagnostics' },
    { '<leader>fr', '<cmd>Telescope resume<cr>', desc = 'Resume' },
    { '<leader>f.', '<cmd>Telescope oldfiles<cr>', desc = 'Recent files' },
    { '<leader>fb', '<cmd>Telescope buffers<cr>', desc = 'Buffers' },

    { '<leader>bb', '<cmd>Telescope buffers<cr>', desc = 'Find buffer' },
  },
  config = function()
    require('telescope').setup {
      defaults = {
        mappings = {
          i = {
            ['<C-p>'] = require('telescope.actions.layout').toggle_preview,
          },
          n = {
            ['<C-p>'] = require('telescope.actions.layout').toggle_preview,
          },
        },
      },
      extensions = {
        ['ui-select'] = {
          require('telescope.themes').get_dropdown(),
        },
      },
      pickers = {
        find_files = {
          path_display = filenameFirst,
        },
        colorscheme = {
          enable_preview = true,
        },
      },
    }

    -- enable telescope extensions, if they are installed
    pcall(require('telescope').load_extension, 'fzf')
    pcall(require('telescope').load_extension, 'ui-select')

    local builtin = require 'telescope.builtin'
    vim.keymap.set('n', '<leader>/', function()
      builtin.current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
        winblend = 10,
        previewer = false,
      })
    end, { desc = '/ Fuzzily search in current buffer' })

    vim.keymap.set('n', '<leader>f/', function()
      builtin.live_grep {
        grep_open_files = true,
        prompt_title = 'Grep open files',
      }
    end, { desc = 'Grep open files' })

    vim.keymap.set('n', '<leader>fn', function()
      builtin.find_files { cwd = vim.fn.stdpath 'config' }
    end, { desc = 'Find Neovim files' })
  end,
}
