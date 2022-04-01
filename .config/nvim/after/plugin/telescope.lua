-- local loaded, telescope = pcall(require, 'telescope')

-- if not loaded then
--   return
-- end

-- telescope.setup()

-- telescope.load_extension 'fzf'
-- telescope.load_extension 'file_browser'

-- local keymap = vim.api.nvim_set_keymap
-- keymap('n', '<leader>pb', [[<cmd> lua require('telescope.builtin').buffers()<cr>]], norem)
-- keymap('n', '<leader>fd', [[<cmd> lua require('telescope.builtin').find_files()<cr>]], norem)
-- keymap('n', '<leader>fb', [[<cmd> lua require('telescope').extensions.file_browser.file_browser()<cr>]], norem)
-- keymap('n', '<leader>fg', [[<cmd> lua require('telescope.builtin').live_grep()<cr>]], norem)
-- keymap('n', '<leader>fh', [[<cmd> lua require('telescope.builtin').help_tags()<cr>]], norem)
-- keymap('n', '<leader>ft', [[<cmd> lua require('telescope.builtin').tags()<cr>]], norem)
-- keymap('n', '<leader>bf', [[<cmd> lua require('telescope.builtin').current_buffer_fuzzy_find()<cr>]], norem)
-- keymap('n', '<leader>bt', [[<cmd> lua require('telescope.builtin').current_buffer_tags()<cr>]], norem)
-- keymap('n', '<leader>tt', [[<cmd> lua require('telescope.builtin').treesitter()<cr>]], norem)
