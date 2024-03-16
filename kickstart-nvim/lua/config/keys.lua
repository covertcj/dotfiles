-- Escape to get rid of search highlight
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous [D]iagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next [D]iagnostic message' })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Show diagnostic [E]rror messages' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })

-- Exit terminal mode easily
vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- Quick window jumps
vim.keymap.set('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
vim.keymap.set('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
vim.keymap.set('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
vim.keymap.set('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

-- Buffers
vim.keymap.set('n', '<leader>bd', ':bd<CR>', { desc = '[D]elete Buffer' })

-- Git
vim.keymap.set('n', '<leader>gg', ':tab Git<CR>', { desc = '[G]it Status' })
vim.keymap.set('n', '<leader>gG', ':vertical rightbelow Git<CR>', { desc = '[G]it Status Vertical Split' })
vim.keymap.set('n', '<leader>gp', ':Git pull<CR>', { desc = '[G]it pull' })
vim.keymap.set('n', '<leader>gP', ':Git push<CR>', { desc = '[G]it push' })
vim.keymap.set('n', '<leader>gf', ':Git fetch<CR>', { desc = '[G]it fetch' })

-- Find
vim.keymap.set('n', '<leader>fo', ':Other<CR>', { desc = '[F]ind Other Files' })
vim.keymap.set('n', '<leader>fO', ':OtherClear<CR>:Other<CR>', { desc = 'Clear and [F]ind Other Files' })
