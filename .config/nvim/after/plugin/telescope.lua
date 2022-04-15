local loaded, telescope = pcall(require, 'telescope')

if not loaded then
  return
end

local themes = require('telescope.themes')
telescope.setup {
  extensions = {
    ['zf-native'] = {
      file = {
        enable = true,
        highlight_results = true,
        match_filename = true,
      },
      generic = {
        enable = true,
        highlight_results = true,
        match_filename = false,
      },
    },
    file_browser = {
      theme = 'ivy',
    },
    frecency = {
      ignore_patterns = {"*.git/*", "*/tmp/*",},
      workspaces = {
        ["conf"] = "~/.config",
      },
    },
  }
}

local extensions = {
  'zf-native',
  'file_browser',
  'frecency',
  'project',
}

for _, ext in ipairs(extensions) do
  telescope.load_extension(ext)
end


local u = require('jw.utils')
local leader = u.leader

leader('sb', [[<cmd> lua require('telescope.builtin').buffers()<cr>]])
leader('pp', telescope.extensions.project.project)
leader('j', telescope.extensions.frecency.frecency)
leader('d', telescope.extensions.file_browser.file_browser)
local builtin = require('telescope.builtin')
leader('fd', [[<cmd> lua require('telescope.builtin').find_files()<cr>]])
leader('fb', [[<cmd> lua require('telescope').extensions.file_browser.file_browser()<cr>]])
leader('fg', builtin.git_files)
leader('gc', builtin.git_bcommits)
leader('gs', builtin.git_stash)
leader('gb', builtin.git_branches)
leader('gC', builtin.git_commits)
leader('sc', builtin.commands)
leader('x', builtin.command_history)
leader('<SPACE>x', builtin.commands)
leader('hh', [[<cmd> lua require('telescope.builtin').help_tags()<cr>]])
leader('hm', [[<cmd> lua require('telescope.builtin').man_pages()<cr>]])
leader('ur', builtin.reloader)
leader('sb', u.partial(builtin.buffers, {ignore_current_buffer = true, sort_mru = true}))
leader('sm', builtin.marks)
leader('sr', builtin.marks)
leader('hk', builtin.keymaps)
leader('ft', [[<cmd> lua require('telescope.builtin').tags()<cr>]])
leader('bf', [[<cmd> lua require('telescope.builtin').current_buffer_fuzzy_find()<cr>]])
leader('bt', [[<cmd> lua require('telescope.builtin').current_buffer_tags()<cr>]])
leader('tt', [[<cmd> lua require('telescope.builtin').treesitter()<cr>]])
