require('nvim-treesitter.configs').setup {
  ensure_installed = { 'go', 'gomod', 'make', 'cmake', 'rst', 'bash', 'comment', 'dockerfile', 'rust', 'toml', 'query', 'javascript', 'typescript', 'haskell', 'regex', 'vim', 'http', 'html', 'css', 'llvm', 'graphql', 'json', 'fennel', 'lua', 'python', 'c', 'cpp', 'yaml'},

  higlight = {
    enable = true,
    disable = {},
    additional_vim_regex_highlighting = false,
  },

  matchup = {
    enable = true,
  },

  refactor = {
    highlight_definitions = { enable = true },
    highlight_current_scope = { enable = true },
  },

  incremental_selection = {
    enable = false,
  },

  context_commentstring = {
    enable = true,
    enable_autocmd = false,
  },

  textsubjects = {
    enable = true,
    -- prev_selection = '<leader>z',
    keymaps = {
      ['<cr>'] = 'textsubjects-smart',
    },
  },

  textobjects = {
    move = {
      enable = true,
      set_jumps = true,

      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = '@class.outer',
      },

      goto_next_end = {
        [']M'] = '@function.outer',
        [']['] = '@class.outer',
      },

      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
      },

      goto_previous_end = {
        ['[M'] = '@function.outer',
        ['[]'] = '@class.outer',
      },
    },

    select = {
      enable = true,
      lookahead = true,

      keymaps = {
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',

        ['ac'] = '@conditional.outer',
        ['ic'] = '@conditional.inner',

        ['aa'] = '@parameter.outer',
        ['ia'] = '@parameter.inner',
      },
    },

    swap = {
      enable = true,

      swap_next = {
        ['<leader>a'] = '@parameter.inner',
      },
      swap_previous = {
        ['<leader>A'] = '@parameter.inner',
      },
    },
  },

  query_linter = {
    enable = true,
    use_virtual_text = true,
    lint_events = {'BufWrite', 'CursorHold'},
  },

  playground = {
    enable = true,
    updatetime = 25,
  },
}
