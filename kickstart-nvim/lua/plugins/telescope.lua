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
      },
    }

    -- enable telescope extensions, if they are installed
    pcall(require('telescope').load_extension, 'fzf')
    pcall(require('telescope').load_extension, 'ui-select')

    local builtin = require 'telescope.builtin'
    vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Help' })
    vim.keymap.set('n', '<leader>fk', builtin.keymaps, { desc = 'Keymaps' })
    vim.keymap.set('n', '<leader><leader>', builtin.find_files, { desc = 'Files' })
    vim.keymap.set('n', '<C-p>', builtin.find_files, { desc = 'Files' })
    vim.keymap.set('n', '<leader>ft', builtin.builtin, { desc = 'Telescope commands' })
    vim.keymap.set('n', '<leader>fw', builtin.grep_string, { desc = 'Word under cursor' })
    vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Grep' })
    vim.keymap.set('n', '<leader>fd', builtin.diagnostics, { desc = 'Diagnostics' })
    vim.keymap.set('n', '<leader>fr', builtin.resume, { desc = 'Resume' })
    vim.keymap.set('n', '<leader>f.', builtin.oldfiles, { desc = 'Recent Files ("." for repeat)' })
    vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Buffers' })

    vim.keymap.set('n', '<leader>bb', builtin.buffers, { desc = 'Find buffer' })
    vim.keymap.set('n', '<leader>tt', builtin.buffers, { desc = 'Theme' })

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
