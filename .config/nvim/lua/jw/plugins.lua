--[[
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim'})
end
--]]

vim.cmd [[packadd packer.nvim]]
local packer = require('packer')

return packer.startup {
  function(use)
    use 'lewis6991/impatient.nvim'
    use {'wbthomason/packer.nvim', opt = true}

    use {
      'tpope/vim-dispatch',
      cmd = {'Dispatch', 'Make', 'Focus', 'Start'}
    }

    use {
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
    }

    use 'nvim-lua/plenary.nvim'
    use 'tpope/vim-repeat'

    local use_sandwich = false
    use { 'tpope/vim-surround', opt = use_sandwich }
    use { 'machakann/vim-sandwich', opt = not use_sandwich }

    use 'ggandor/lightspeed.nvim'
    use 'AndrewRadev/splitjoin.vim'

    use 'mfussenegger/nvim-lint'
    use 'tpope/vim-fugitive'
    use { 'tpope/vim-rhubarb', cmd = 'GBrowse' }
    use 'tpope/vim-commentary'
    use 'tpope/vim-abolish'
    use 'tpope/vim-characterize'
    use 'tpope/vim-apathy'
    use 'tpope/vim-sleuth'
    use 'tpope/vim-projectionist'
    use 'tpope/vim-obsession'

    use { 'editorconfig/editorconfig-vim', opt = true }

    use {
      'gpanders/nvim-parinfer',
      config = [[vim.g.parinfer_no_maps = 1]]
    }

    use 'fladson/vim-kitty'

    use 'romainl/vim-qlist'
    use { 'romainl/vim-qf', config = [[vim.g.qf_mapping_ack_style = 1]] }
    use { 'andymass/vim-matchup', config = [[vim.g.matchparen_offscreen = {}]] }
    use { 'wellle/targets.vim', opt = true }

    use 'haya14busa/vim-asterisk'
    use 'kevinhwang91/nvim-hlslens'

    use { 'rhysd/committia.vim' }

    use { 'sindrets/diffview.nvim', requires = 'nvim-lua/plenary.nvim' }

    use {
      'folke/which-key.nvim',
      opt = true,
    }

    use {
      'TimUntersberger/neogit',
      requires = { 'nvim-lua/plenary.nvim', 'sindrets/diffview.nvim' },
      opt = true, -- maybe once this has more features
    }

    use 'milisims/nvim-luaref'
    use 'nanotee/luv-vimdocs'

    use { 'neovim/nvim-lspconfig' }
    use { 'onsails/lspkind-nvim' }

    use { 'hrsh7th/cmp-nvim-lsp' }
    use { 'hrsh7th/cmp-buffer' }
    use { 'hrsh7th/cmp-path' }
    use { 'hrsh7th/nvim-cmp' }

    use { 'L3MON4D3/LuaSnip' }
    use { 'saadparwaiz1/cmp_luasnip' }
    use { 'rafamadriz/friendly-snippets' }

    use { 'kevinhwang91/nvim-bqf', opt = true }

    use {
      'folke/trouble.nvim',
      requires = 'kyazdani42/nvim-web-devicons',
    }

    use 'gennaro-tedesco/nvim-jqx'

    use {
      'folke/tokyonight.nvim',
      config = function()
        vim.g.tokyonight_style = 'night'
        vim.g.tokyonight_sidebars = { 'qf', 'vista_kind', 'terminal', 'packer' }
        vim.cmd 'colorscheme tokyonight'
      end,
      event = 'UIEnter',
    }

    use {
     'kyazdani42/nvim-web-devicons',
      config = {
        default = true,
      },
      after = 'tokyonight.nvim',
    }

    use {
      'lewis6991/gitsigns.nvim',
      requires = 'nvim-lua/plenary.nvim',
      config = function()
        require('gitsigns').setup {
          signs = {
            add = {
              hl = 'GitGutterAdd',
              text = '+',
            },
            change = {
              hl = 'GitGutterChange',
              text = '~',
            },
            delete = {
              hl = 'GitGutterDelete',
              text = '_',
            },
            topdelete = {
              hl = 'GitGutterDelete',
              text = '‾',
            },
            changedelete = {
              hl = 'GitGutterChange',
              text = '~',
            },
          },
        }
      end,
    }

    local use_telescope = false

    use {
      'nvim-telescope/telescope.nvim',
      requires = {'nvim-lua/plenary.nvim', 'nvim-lua/popup.nvim'},
      opt = not use_telescope,
    }

    use {
      'nvim-telescope/telescope-fzf-native.nvim',
      opt = not use_telescope,
      run = 'make',
      requires = 'nvim-telescope/telescope.nvim',
    }

    use {
      'nvim-telescope/telescope-file-browser.nvim',
      opt = not use_telescope,
      requires = 'nvim-telescope/telescope.nvim',
    }

    use {
      'ibhagwan/fzf-lua',
      opt = use_telescope,
    }



    use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
    use {
      'nvim-treesitter/playground',
      requires = 'nvim-treesitter/nvim-treesitter',
      cmd = 'TSPlaygroundToggle',
    }
    use {'nvim-treesitter/nvim-treesitter-textobjects', requires = 'nvim-treesitter/nvim-treesitter'}
    use {'RRethy/nvim-treesitter-textsubjects', requires = 'nvim-treesitter/nvim-treesitter'}
    use {'JoosepAlviste/nvim-ts-context-commentstring', requires = 'nvim-treesitter/nvim-treesitter'}

    use {'dstein64/vim-startuptime', cmd = 'StartupTime',}

    use { 'karb94/neoscroll.nvim', config = [[require('neoscroll').setup()]] }

    use {
      'lukas-reineke/indent-blankline.nvim',
      after = 'tokyonight.nvim',
      config = function()
        local blankline = require('indent_blankline')
        blankline.setup {
          char_list = {'|', '¦', '┆', '┊'},
          filetype_exclude = {
            'help',
            'packer',
            'man',
            'lspinfo',
            'diagnosticpopup',
          },
          buftype_exclude = { 'terminal', 'nofile', },
          space_char_blankline = " ",
          show_current_context = true,
          show_current_context_start = true,
          show_foldtext = false,
          show_end_of_line = true,
          disable_with_nolist = true,
        }
      end,
    }

    use { 'ziglang/zig.vim', ft = 'zig' }

  end,
  config = {
    profile = {
      enable = false,
      threshold = 5,
    },
  },
}
