return {
  'nvim-neotest/neotest',
  dependencies = {
    'nvim-neotest/nvim-nio',
    'nvim-lua/plenary.nvim',
    'antoinemadec/FixCursorHold.nvim',
    'nvim-treesitter/nvim-treesitter',

    -- adapters
    'nvim-neotest/neotest-jest',
  },
  config = function()
    vim.api.nvim_set_keymap('n', '<leader>tw', "<cmd>lua require('neotest').run.run({ jestCommand = 'yarn run test:watch --' })<cr>", {})

    require('neotest').setup {
      adapters = {
        require 'neotest-jest' {
          jestCommnad = 'yarn run test --',
          jest_test_discovery = false,
          cwd = function(file)
            if string.find(file, '/packages') then
              return string.match(file, '(.-/[^/]+/)src')
            end

            return vim.fn.getcwd()
          end,
          jestConfigFile = function(file)
            if string.find(file, '/packages/') then
              return string.match(file, '(.-/[^/]+/)src') .. 'jest.config.ts'
            end

            return vim.fn.getcwd() .. '/jest.config.ts'
          end,
        },
      },
    }
  end,
}
