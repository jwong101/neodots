local ok, formatter = pcall(require, 'formatter')
if not ok then
  return
end

local util = require 'formatter.util'
local defaults = require 'formatter.defaults'
local clangformat = util.copyf(defaults.clangformat)
local prettierd = util.copyf(defaults.prettierd)
local zigfmt = util.copyf(defaults.zigfmt)

formatter.setup {
  logging = true,
  log_level = vim.log.levels.WARN,
  filetype = {
    c = {
      clangformat,
    },
    cpp = {
      clangformat,
    },

    rust = {
      function()
        return {
          exe = 'rustfmt --edition 2021',
          stdin = true,
        }
      end,
    },

    javascript = {
      prettierd,
    },

    typescript = {
      prettierd,
    },

    html = {
      prettierd,
    },

    css = {
      prettierd,
    },

    javascriptreact = {
      prettierd,
    },

    typescriptreact = {
      prettierd,
    },

    json = {
      prettierd,
    },

    yaml = {
      prettierd,
    },

    python = {
      function()
        return {
          exe = 'black',
          args = { '--preview', '-q', '-' },
          stdin = true,
        }
      end,
      function()
        return {
          exe = 'isort',
          args = { '--profile', 'black', '-q', '-' },
          stdin = true,
        }
      end,
    },

    lua = {
      require('formatter.filetypes.lua').stylua,
      function()
        return {
          exe = 'stylua',
          args = {
            '--search-parent-directories',
            '--stdin-filepath',
            util.escape_path(util.get_current_buffer_file_path()),
            '--',
            '-',
          },
          stdin = true,
        }
      end,
    },

    zig = {
      zigfmt,
    },

    ['*'] = {
      require('formatter.filetypes.any').remove_trailing_whitespace,
    },
  },
}

local api = vim.api
local format = require('formatter.format').format
local invoke_format = function(_)
  local max_lines = api.nvim_buf_line_count(0)
  format('', '', 1, max_lines, { write = true })
end

local id = api.nvim_create_augroup('FormatAugroup', {})
api.nvim_create_autocmd({ 'BufWritePost' }, {
  group = id,
  pattern = { '*' },
  callback = invoke_format,
})
