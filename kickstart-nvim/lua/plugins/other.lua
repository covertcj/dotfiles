return {
  'rgroli/other.nvim',
  cmd = { 'Other', 'OtherClean' },
  config = function()
    require('other-nvim').setup {
      hooks = {
        -- filter out files that don't exist
        onFindOtherFiles = function(matches)
          for key, match in pairs(matches) do
            if not match.exists then
              matches[key] = nil
            end
          end

          return matches
        end,
      },
      mappings = {
        {
          pattern = '(.*)/__tests__/(.*)%.(test|spec)%.[jt]sx?$',
          target = {
            {
              target = '%1/%2.js',
              context = 'impl',
            },
            {
              target = '%1/%2.jsx',
              context = 'impl',
            },
            {
              target = '%1/%2.ts',
              context = 'impl',
            },
            {
              target = '%1/%2.tsx',
              context = 'impl',
            },
          },
        },
        {
          pattern = '(.*)/(.*)%.[tj]sx?$',
          target = {
            -- .spec.*
            {
              target = '%1/__tests__/%2.spec.js',
              context = 'test',
            },
            {
              target = '%1/__tests__/%2.spec.jsx',
              context = 'test',
            },
            {
              target = '%1/__tests__/%2.spec.ts',
              context = 'test',
            },
            {
              target = '%1/__tests__/%2.spec.tsx',
              context = 'test',
            },

            -- .test.*
            {
              target = '%1/__tests__/%2.test.js',
              context = 'test',
            },
            {
              target = '%1/__tests__/%2.test.jsx',
              context = 'test',
            },
            {
              target = '%1/__tests__/%2.test.ts',
              context = 'test',
            },
            {
              target = '%1/__tests__/%2.test.tsx',
              context = 'test',
            },

            -- _test.*
            {
              target = '%1/%2_test.js',
              context = 'test',
            },
            {
              target = '%1/%2_test.jsx',
              context = 'test',
            },
            {
              target = '%1/%2_test.ts',
              context = 'test',
            },
            {
              target = '%1/%2_test.tsx',
              context = 'test',
            },
          },
        },
      },
    }
  end,
}
