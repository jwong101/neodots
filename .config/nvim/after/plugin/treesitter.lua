require('orgmode').setup_ts_grammar()
vim.g.indent_blankline_use_treesitter = true
require('nvim-treesitter.configs').setup {
  ensure_installed = {
    'agda',
    'awk',
    'bash',
    'bibtex',
    'c',
    'cmake',
    'comment',
    'commonlisp',
    'cpp',
    'css',
    'csv',
    'cuda',
    'devicetree',
    'diff',
    'dockerfile',
    'dot',
    'doxygen',
    'ebnf',
    'embedded_template',
    'fennel',
    'glsl',
    'gitattributes',
    'gitcommit',
    'gitignore',
    'git_config',
    'git_rebase',
    'go',
    'gomod',
    'gowork',
    'gpg',
    'graphql',
    'haskell',
    'hcl',
    'html',
    'http',
    'ini',
    'java',
    'javascript',
    'jsdoc',
    'json',
    'jsonc',
    'jq',
    'lalrpop',
    'latex',
    'llvm',
    'lua',
    'luadoc',
    'luap',
    'make',
    'markdown',
    'markdown_inline',
    'menhir',
    'mermaid',
    'meson',
    'ninja',
    'nix',
    'ocaml',
    'ocaml_interface',
    'ocamllex',
    'org',
    'pascal',
    'passwd',
    'pem',
    'python',
    'ql',
    'query',
    'regex',
    'requirements',
    'rst',
    'rust',
    'scss',
    'sparql',
    'sql',
    'svelte',
    'teal',
    'toml',
    'tsx',
    'typescript',
    'vim',
    'yaml',
    'zig',
  },

  highlight = {
    enable = true,
  },

  rainbow = {
    enable = true,
    query = 'rainbow-parens',
    strategy = require 'ts-rainbow.strategy.global',
  },

  matchup = { enable = true },

  refactor = {
    highlight_definitions = { enable = true },
    highlight_current_scope = { enable = true },
  },

  incremental_selection = { enable = false },

  context_commentstring = { enable = true, enable_autocmd = false },

  autotag = { enable = true },

  textsubjects = {
    enable = true,
    -- prev_selection = '<leader>z',
    keymaps = { ['<cr>'] = 'textsubjects-smart' },
  },

  textobjects = {
    move = {
      enable = true,
      set_jumps = true,

      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = '@class.outer',
      },

      goto_next_end = { [']M'] = '@function.outer', [']['] = '@class.outer' },

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

      keymaps = { ['af'] = '@function.outer', ['if'] = '@function.inner' },
    },

    swap = {
      enable = true,

      swap_next = { ['<leader>a'] = '@parameter.inner' },
      swap_previous = {
        ['<leader>A'] = '@parameter.inner',
      },
    },
  },

  query_linter = {
    enable = true,
    use_virtual_text = true,
    lint_events = { 'BufWrite', 'CursorHold' },
  },

  playground = { enable = true, updatetime = 25 },
}

local context = require 'treesitter-context'
context.setup {
  enable = true,
}

vim.cmd [[omap <silent> m :<C-U>lua require('tsht').nodes()<CR>]]
vim.cmd [[xnoremap <silent> m :lua require('tsht').nodes()<CR>]]
