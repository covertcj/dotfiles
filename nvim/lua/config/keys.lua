-- Escape to get rid of search highlight
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Previous diagnostic' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Next Diagnostic' })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Show diagnostics here' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic list' })

-- Exit terminal mode easily
vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- Quick window jumps
vim.keymap.set('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
vim.keymap.set('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
vim.keymap.set('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
vim.keymap.set('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

-- Makes the current window take up as much space as possible, or equally space
-- out all windows if it was already full screen.
--
-- This is done by checking to see if the current window already looks like its
-- taking up most of the space or not.
local function fullscreen_window()
  local total_width = vim.api.nvim_get_option 'columns'
  local total_height = vim.api.nvim_get_option 'lines'

  local win_width = vim.api.nvim_win_get_width(0)
  local win_height = vim.api.nvim_win_get_height(0)

  local width_percentage = (win_width / total_width)
  local height_percentage = (win_height / total_height)

  if width_percentage > 0.8 and height_percentage > 0.8 then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<C-w>=', true, true, true), 'n', false)
  else
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<C-w>|<C-w>_', true, true, true), 'n', false)
  end
end

vim.keymap.set('n', '<C-w>f', fullscreen_window, { desc = 'Fullscreen window' })
