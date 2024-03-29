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

-- Buffers
vim.keymap.set('n', '<leader>bd', ':bd<CR>', { desc = 'Delete buffer' })

-- Git
vim.keymap.set('n', '<leader>gg', ':tab Git<CR>', { desc = 'Git status' })
vim.keymap.set('n', '<leader>gG', ':vertical rightbelow Git<CR>', { desc = 'Git status split' })
vim.keymap.set('n', '<leader>gp', ':Git pull<CR>', { desc = 'Git pull' })
vim.keymap.set('n', '<leader>gP', ':Git push<CR>', { desc = 'Git push' })
vim.keymap.set('n', '<leader>gf', ':Git fetch<CR>', { desc = 'Git fetch' })

-- Find
vim.keymap.set('n', '<leader>fo', ':Other<CR>', { desc = 'Find other files' })
vim.keymap.set('n', '<leader>fO', ':OtherClear<CR>:Other<CR>', { desc = 'Clear and find other files' })
