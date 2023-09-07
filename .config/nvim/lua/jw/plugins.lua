local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

-- vim.cmd [[packadd packer.nvim]]

-- package.loaded['packer'] = nil
-- local packer = require 'packer'

local lazy = require 'lazy'

return lazy.setup({

  {
    'tpope/vim-dispatch',
    cmd = { 'Dispatch', 'Make', 'Focus', 'Start' },
  },

  {
    'tpope/vim-scriptease',
    cmd = {
      'Messages',
      'Verbose',
      'Time',
      'PP',
      'Breakadd',
      'Vedit',
      'Disarm',
    },
  },

  { 'nvim-lua/plenary.nvim' },
  { 'tpope/vim-repeat' },

  { 'tpope/vim-surround' },

  {
    'ggandor/leap.nvim',
    config = function()
      require('leap').set_default_keymaps()
    end,
  },

  {
    'AndrewRadev/splitjoin.vim',
    keys = { 'gJ', 'gS' },
  },

  {
    'dnlhc/glance.nvim',
    config = function()
      require('glance').setup()
    end,
  },

  { 'mfussenegger/nvim-lint' },
  { 'tpope/vim-fugitive' },
  { 'tpope/vim-rhubarb', cmd = 'GBrowse' },

  { 'tpope/vim-abolish' },
  { 'tpope/vim-characterize' },
  { 'tpope/vim-apathy' },
  { 'tpope/vim-sleuth' },
  { 'tpope/vim-projectionist' },
  { 'tpope/vim-obsession' },
  { 'tpope/vim-rsi' },

  {
    'tpope/vim-eunuch',
    cmd = {
      'Delete',
      'Unlink',
      'Move',
      'Rename',
      'Chmod',
      'Mkdir',
      'Cfind',
      'Clocate',
      'Lfind',
      'Llocate',
      'Wall',
      'SudoWrite',
      'SudoEdit',
    },
  },

  { 'tpope/vim-dadbod' },
  { 'kristijanhusak/vim-dadbod-ui' },
  { 'kristijanhusak/vim-dadbod-completion' },

  { 'justinmk/vim-dirvish' },
  { 'kristijanhusak/vim-dirvish-git', ft = 'dirvish' },
  { 'fsharpasharp/vim-dirvinist' },

  {
    'monaqa/dial.nvim',
    config = function()
      local vmap = require('jw.utils').vmap
      local nmap = require('jw.utils').nmap
      local dmap = require 'dial.map'
      nmap('<C-a>', dmap.inc_normal())
      nmap('<C-x>', dmap.dec_normal())
      vmap('<C-a>', dmap.inc_visual())
      vmap('<C-X>', dmap.dec_visual())
      vmap('g<C-a>', dmap.inc_gvisual())
      vmap('g<C-X>', dmap.dec_gvisual())
    end,
  },

  {
    'numToStr/Comment.nvim',
  },

  { 'fladson/vim-kitty' },

  { 'romainl/vim-qlist' },
  { 'romainl/vim-qf' },
  { 'andymass/vim-matchup', config = [[vim.g.matchparen_offscreen = {}]] },
  { 'wellle/targets.vim' },
  { 'michaeljsmith/vim-indent-object' },
  { 'tommcdo/vim-exchange' },

  { 'nvim-orgmode/orgmode', config = [[require("jw.orgmode")]] },

  {
    'akinsho/org-bullets.nvim',
    ft = 'org',
    config = function()
      require('org-bullets').setup {
        symbols = { '◉', '○', '✸', '✿' },
      }
    end,
  },

  {
    'folke/todo-comments.nvim',
    dependencies = 'nvim-lua/plenary.nvim',
    config = function()
      local todo = require 'todo-comments'
      todo.setup()
      vim.keymap.set(
        'n',
        ']<C-t>',
        todo.jump_next,
        { silent = true, desc = 'Next todo comment' }
      )
      vim.keymap.set(
        'n',
        '[<C-t>',
        todo.jump_prev,
        { silent = true, desc = 'Prev todo comment' }
      )
    end,
  },

  {
    'bennypowers/nvim-regexplainer',
    dependencies = { 'nvim-treesitter/nvim-treesitter', 'MunifTanjim/nui.nvim' },
    config = function()
      require('regexplainer').setup {
        mappings = {
          toggle = '<leader>hR',
        },
      }
    end,
  },

  {
    'lukas-reineke/headlines.nvim',
    config = function()
      require('headlines').setup()
    end,
  },

  { 'haya14busa/vim-asterisk' },
  {
    'kevinhwang91/nvim-hlslens',
    config = function()
      require('hlslens').setup {
        calm_down = true,
      }
    end,
  },

  { 'rhysd/committia.vim' },

  { 'sindrets/diffview.nvim', dependencies = 'nvim-lua/plenary.nvim' },
  { 'rhysd/git-messenger.vim' },

  { 'neovim/nvim-lspconfig' },
  { 'onsails/lspkind-nvim' },
  { 'b0o/schemastore.nvim' },
  { 'folke/lsp-colors.nvim' },

  { 'https://git.sr.ht/~whynothugo/lsp_lines.nvim', lazy = true },

  {
    'simrat39/rust-tools.nvim',
    dependencies = { 'nvim-lspconfig', 'plenary.nvim', 'nvim-dap' },
  },

  {
    'weilbith/nvim-code-action-menu',
    cmd = 'CodeActionMenu',
  },

  {
    'Saecki/crates.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'jose-elias-alvarez/null-ls.nvim',
    },
    config = function()
      require('crates').setup {
        disable_invalid_feature_diagnostic = true,
      }
    end,
  },

  {
    'RubixDev/ebnf',
    config = function(plugin)
      vim.opt.rtp:append(plugin.dir .. '/crates/tree-sitter-ebnf')
      vim.filetype.add { extension = { ebnf = 'ebnf' } }
    end,
  },

  {
    'jose-elias-alvarez/nvim-lsp-ts-utils',
    dependencies = 'plenary.nvim',
  },

  {
    'j-hui/fidget.nvim',
    config = function()
      require('fidget').setup {
        sources = {
          ['null-ls'] = {
            ignore = true,
          },
        },
      }
    end,
  },

  { 'hrsh7th/nvim-cmp' },
  { 'hrsh7th/cmp-nvim-lua' },
  { 'hrsh7th/cmp-buffer' },
  { 'hrsh7th/cmp-nvim-lsp' },
  { 'hrsh7th/cmp-nvim-lsp-signature-help' },
  { 'hrsh7th/cmp-path' },

  { 'L3MON4D3/LuaSnip' },
  { 'saadparwaiz1/cmp_luasnip' },
  { 'rafamadriz/friendly-snippets' },
  { 'windwp/nvim-autopairs' },

  {
    'kosayoda/nvim-lightbulb',
    config = [[require"nvim-lightbulb".setup()]],
  },

  {
    'neovimhaskell/haskell-vim',
    ft = 'haskell',
  },

  {
    'elzr/vim-json',
    ft = 'json',
  },

  {
    'folke/trouble.nvim',
    dependencies = 'kyazdani42/nvim-web-devicons',
  },

  {
    'rcarriga/nvim-notify',
  },

  {
    'gennaro-tedesco/nvim-jqx',
  },

  {
    'rebelot/kanagawa.nvim',
    lazy = false,
    priority = 1000,
    config = function()
      require('kanagawa').setup {
        compile = true,
        dimInactive = true,
        globalStatus = true,
        transparent = true,
      }
      vim.cmd.colorscheme 'kanagawa'
    end,
    event = 'UIEnter',
  },

  {
    'kyazdani42/nvim-web-devicons',
    opts = {
      default = true,
    },
  },

  {
    'yamatsum/nvim-nonicons',
    dependencies = { 'kyazdani42/nvim-web-devicons' },
  },

  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'kyazdani42/nvim-web-devicons' },
  },

  {
    'lewis6991/gitsigns.nvim',
    dependencies = 'nvim-lua/plenary.nvim',
  },

  {
    'mfussenegger/nvim-dap',
    config = function()
      local dap = require 'dap'
      local u = require 'jw.utils'
      u.nmap('<leader>db', dap.toggle_breakpoint)
      u.nmap('<leader>dB', function()
        dap.toggle_breakpoint(
          vim.fn.input 'Breakpoint Condition: ',
          nil,
          nil,
          true
        )
      end)
      u.nmap('<leader>dr', function()
        dap.repl.toggle { height = 15 }
      end)
    end,
  },

  {
    'rcarriga/nvim-dap-ui',
    dependencies = 'nvim-dap',
  },

  {
    'theHamsta/nvim-dap-virtual-text',
    config = function()
      require('nvim-dap-virtual-text').setup {
        commented = true,
      }
    end,
  },

  { 'mhartington/formatter.nvim' },

  {
    'nvim-telescope/telescope.nvim',
    dependencies = { 'nvim-lua/plenary.nvim', 'nvim-lua/popup.nvim' },
  },

  {
    'nvim-telescope/telescope-frecency.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim', 'tami5/sqlite.lua' },
  },

  {
    'nvim-telescope/telescope-live-grep-args.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim' },
  },

  {
    'stevearc/dressing.nvim',
    event = 'VeryLazy',
  },

  {
    'andrewferrier/textobj-diagnostic.nvim',
    config = function()
      require('textobj-diagnostic').setup()
    end,
  },

  {
    'natecraddock/telescope-zf-native.nvim',
  },

  {
    'nvim-telescope/telescope-project.nvim',
  },

  {
    'nvim-telescope/telescope-file-browser.nvim',
    dependencies = 'nvim-telescope/telescope.nvim',
  },

  { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },

  {
    'nvim-treesitter/playground',
    dependencies = 'nvim-treesitter/nvim-treesitter',
    cmd = 'TSPlaygroundToggle',
  },

  {
    'nvim-treesitter/nvim-treesitter-textobjects',
    dependencies = 'nvim-treesitter/nvim-treesitter',
  },

  {
    'RRethy/nvim-treesitter-textsubjects',
    dependencies = 'nvim-treesitter/nvim-treesitter',
  },

  {
    'nvim-treesitter/nvim-treesitter-context',
    dependencies = 'nvim-treesitter/nvim-treesitter',
  },

  {
    'mfussenegger/nvim-treehopper',
    dependencies = 'nvim-treesitter/nvim-treesitter',
  },

  {
    'JoosepAlviste/nvim-ts-context-commentstring',
    dependencies = 'nvim-treesitter/nvim-treesitter',
  },

  {
    'windwp/nvim-ts-autotag',
    dependencies = 'nvim-treesitter/nvim-treesitter',
  },

  {
    'https://gitlab.com/HiPhish/nvim-ts-rainbow2',
    dependencies = 'nvim-treesitter/nvim-treesitter',
  },

  {
    'danymat/neogen',
    dependencies = 'nvim-treesitter/nvim-treesitter',
  },

  {
    'mizlan/iswap.nvim',
    config = function()
      require('iswap').setup {
        autoswap = true,
      }
      vim.keymap.set('n', '<leader>cx', '<Plug>ISwapWith', { remap = true })
      vim.keymap.set('n', '<leader>cX', '<Plug>ISwapNormal', { remap = true })
    end,
  },

  { 'dstein64/vim-startuptime', cmd = 'StartupTime' },

  {
    'karb94/neoscroll.nvim',
    config = function()
      require('neoscroll').setup()
    end,
  },

  { 'edluffy/hologram.nvim' },

  {
    'cuducos/yaml.nvim',
    ft = { 'yaml' },
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
      'nvim-telescope/telescope.nvim',
    },
  },

  -- {
  --   'github/copilot.vim',
  --   config = function()
  --     vim.g.copilot_no_tab_map = 1
  --   end,
  -- }

  {
    'zbirenbaum/copilot.lua',
    event = { 'VimEnter' },
    config = function()
      vim.defer_fn(function()
        require('copilot').setup {
          cmp = {
            enabled = true,
            method = 'getCompletionsCycling',
            autofmt = true,
          },
          panel = {
            enabled = true,
          },
          ft_disable = { 'markdown' },
        }
      end, 100)
    end,
  },

  {
    'zbirenbaum/copilot-cmp',
    dependencies = 'copilot.lua',
    config = function()
      require('copilot_cmp').setup {
        formatters = {
          insert_text = require('copilot_cmp.format').remove_existing,
        },
      }
    end,
  },

  {
    'kevinhwang91/nvim-ufo',
    dependencies = 'kevinhwang91/promise-async',
  },

  {
    'lukas-reineke/indent-blankline.nvim',
    config = function()
      local blankline = require 'indent_blankline'
      vim.g.indent_blankline_use_treesitter = true
      blankline.setup {
        char_list = { '|', '¦', '┆', '┊' },
        filetype_exclude = {
          'help',
          'packer',
          'man',
          'lspinfo',
          'checkhealth',
          'Trouble',
          'diagnosticpopup',
        },
        buftype_exclude = { 'terminal', 'nofile' },
        space_char_blankline = ' ',
        show_current_context = true,
        show_current_context_start = true,
        show_foldtext = false,
        show_end_of_line = true,
        disable_with_nolist = true,
      }
    end,
  },
  { 'ziglang/zig.vim', ft = 'zig' },

  { 'NoahTheDuke/vim-just' },
}, {
  diff = {
    cmd = 'diffview.nvim',
  },
})
