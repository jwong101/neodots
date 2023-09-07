local loaded, telescope = pcall(require, 'telescope')

if not loaded then
  return
end

local themes = require 'telescope.themes'
local lga_actions = require 'telescope-live-grep-args.actions'
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
    project = {
      base_dirs = {
        '~/projects',
        '~/workspace',
      },
    },
    file_browser = {
      theme = 'ivy',
    },
    frecency = {
      ignore_patterns = { '*.git/*', '*/tmp/*' },
      workspaces = {
        ['conf'] = '~/.config',
        ['projects'] = '~/projects',
        ['workspace'] = '~/workspace',
      },
    },

    live_grep_args = {
      auto_quoting = true,
      mappings = {
        i = {
          ['<C-t>'] = lga_actions.quote_prompt { postfix = ' -t' },
        },
      },
    },
  },
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

local u = require 'jw.utils'
local leader = u.leader
local utils = require 'telescope.utils'
local ivy = themes.get_ivy()

-- leader('sb', [[<cmd> lua require('telescope.builtin').buffers()<cr>]])
leader('pp', telescope.extensions.project.project)
leader('j', telescope.extensions.frecency.frecency)
leader('d', telescope.extensions.file_browser.file_browser)
local builtin = require 'telescope.builtin'
leader('fd', [[<cmd> lua require('telescope.builtin').find_files()<cr>]])
local buffer_find_files = function()
  builtin.find_files {
    cwd = utils.buffer_dir(),
  }
end
leader('<leader>fd', buffer_find_files)
leader(
  'fb',
  [[<cmd> lua require('telescope').extensions.file_browser.file_browser()<cr>]]
)
leader('rr', builtin.resume)
leader('fg', builtin.git_files)

-- <cr> run git apply for stash
leader('gs', builtin.git_stash)

-- <cr> check out branch
-- <c-t> tracks selected branch
-- <c-r> rebases selected branch
-- <c-a> creates new branch w/ confirmation prompt
-- <c-d> deletes selected branch w/ confirmation
-- <c-y> merges selected branch w/ confirmation
leader('gb', builtin.git_branches)

-- use <cr> to check out commit
-- use <C-r>[m, s, h] to reset branch to selected commit (hard, soft mixed)
leader('gc', builtin.git_commits)

-- <cr> checks out currently selected commit
-- <C-[x,v,t]> open diff in horizontal, vertical split, or tab
leader('<leader>gc', builtin.git_bcommits)

-- use <C-e> to send command or search to command line but don't execute it
leader('sc', u.partial(builtin.commands, ivy))

leader('xh', u.partial(builtin.command_history, ivy))

-- leader('<SPACE>x', builtin.commands)
leader('hh', [[<cmd> lua require('telescope.builtin').help_tags()<cr>]])
leader('hm', [[<cmd> lua require('telescope.builtin').man_pages()<cr>]])
leader('hk', u.partial(builtin.keymaps, ivy))
leader('ur', builtin.reloader)
leader(
  'sb',
  u.partial(builtin.buffers, { ignore_current_buffer = true, sort_mru = true })
)
leader('rg', telescope.extensions.live_grep_args.live_grep_args)
leader('<leader>rg', function()
  telescope.extensions.live_grep_args.live_grep_args { cwd = utils.buffer_dir() }
end)
leader('*', builtin.grep_string)
leader('#', function()
  builtin.grep_string { cwd = vim.fs.dirname(vim.api.nvim_buf_get_name(0)) }
end)
leader('sm', builtin.marks)
-- use <C-e> to edit the contents of the registers
leader('sr', builtin.registers)
leader('ft', [[<cmd> lua require('telescope.builtin').tags()<cr>]])
leader(
  'ff',
  [[<cmd> lua require('telescope.builtin').current_buffer_fuzzy_find()<cr>]]
)
leader(
  'bt',
  [[<cmd> lua require('telescope.builtin').current_buffer_tags()<cr>]]
)
leader('co', [[<cmd> lua require('telescope.builtin').treesitter()<cr>]])
