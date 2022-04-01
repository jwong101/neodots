--[[
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim'})
end
--]]

vim.cmd [[packadd packer.nvim]]
package.loaded["packer"] = nil
local packer = require('packer')

return packer.startup {
  function(use)
    use 'lewis6991/impatient.nvim'
    use { 'wbthomason/packer.nvim', opt = true }

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

    use { 'tpope/vim-surround' }

    use 'ggandor/lightspeed.nvim'

    use {
      'AndrewRadev/splitjoin.vim',
      keys = { 'gJ', 'gS' },
    }

    use 'mfussenegger/nvim-lint'
    use 'tpope/vim-fugitive'
    use { 'tpope/vim-rhubarb', cmd = 'GBrowse' }
    use { 'tpope/vim-commentary', opt = true }
    use 'tpope/vim-abolish'
    use 'tpope/vim-characterize'
    use 'tpope/vim-apathy'
    use 'tpope/vim-sleuth'
    use 'tpope/vim-projectionist'
    use 'tpope/vim-obsession'
    use 'tpope/vim-rsi'
    use {
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
    }
    use 'tpope/vim-dadbod'
    use 'kristijanhusak/vim-dadbod-ui'
    use 'kristijanhusak/vim-dadbod-completion'

    use 'justinmk/vim-dirvish'
    use { 'kristijanhusak/vim-dirvish-git', ft = 'dirvish' }
    use { 'fsharpasharp/vim-dirvinist' }

    use {
      'monaqa/dial.nvim',
      config = function()
        local nvmap = function (key, def)
          return vim.keymap.set({'n', 'v'}, key, def, { remap = true, })
        end
        local vmap = require('jw.utils').vmap
        nvmap("<C-a>", "<Plug>(dial-increment)")
        nvmap("<C-x>", "<Plug>(dial-decrement)")
        local dmap = require('dial.map')
        vmap("g<C-a>", dmap.inc_gvisual())
        vmap("g<C-X>", dmap.dec_gvisual())
      end,
    }

    use {
      'numToStr/Comment.nvim',
    }

    use { 'editorconfig/editorconfig-vim', opt = true }

    use {
      'gpanders/nvim-parinfer',
      opt = true,
      config = [[vim.g.parinfer_no_maps = 1]],
    }

    use 'fladson/vim-kitty'

    use 'romainl/vim-qlist'
    use { 'romainl/vim-qf', config = [[vim.g.qf_mapping_ack_style = 1]] }
    use { 'andymass/vim-matchup', config = [[vim.g.matchparen_offscreen = {}]] }
    use { 'wellle/targets.vim' }

    use 'haya14busa/vim-asterisk'
    use 'kevinhwang91/nvim-hlslens'

    use { 'rhysd/committia.vim' }

    use { 'sindrets/diffview.nvim', requires = 'nvim-lua/plenary.nvim' }

    use 'milisims/nvim-luaref'
    use 'nanotee/luv-vimdocs'

    use { 'neovim/nvim-lspconfig' }
    use { 'onsails/lspkind-nvim' }
    use { 'b0o/schemastore.nvim' }
    use 'ray-x/lsp_signature.nvim'
    use 'folke/lsp-colors.nvim'
    use {
      'jose-elias-alvarez/nvim-lsp-ts-utils',
      requires = 'plenary.nvim',
    }
    use {
      'j-hui/fidget.nvim',
      after = 'kanagawa.nvim',
      config = [[require"fidget".setup()]],
    }

    use { 'hrsh7th/cmp-nvim-lsp' }
    use 'hrsh7th/cmp-nvim-lua'
    use { 'hrsh7th/cmp-buffer' }
    use { 'hrsh7th/cmp-path' }
    use { 'hrsh7th/nvim-cmp' }

    use { 'L3MON4D3/LuaSnip' }
    use { 'saadparwaiz1/cmp_luasnip' }
    use { 'rafamadriz/friendly-snippets' }
    use { 'windwp/nvim-autopairs' }

    use {
      'neovimhaskell/haskell-vim',
      ft = 'haskell',
    }

    use {
      'elzr/vim-json',
      ft = 'json',
    }

    use {
      'folke/trouble.nvim',
      requires = 'kyazdani42/nvim-web-devicons',
    }
    use {
      'rcarriga/nvim-notify',
    }

    use 'gennaro-tedesco/nvim-jqx'

    use {
      'folke/tokyonight.nvim',
      config = function()
        vim.g.tokyonight_style = 'night'
        vim.g.tokyonight_sidebars = { 'qf', 'vista_kind', 'terminal', 'packer' }
        vim.cmd 'colorscheme tokyonight'
      end,
      opt = true,
    }

    use {
      'rebelot/kanagawa.nvim',
      config = function()
        vim.cmd "colorscheme kanagawa"
      end,
      event = 'UIEnter',
    }

    use {
     'kyazdani42/nvim-web-devicons',
      config = {
        default = true,
      },
      after = 'kanagawa.nvim',
    }

    use {
      'yamatsum/nvim-nonicons',
      requires = {'kyazdani42/nvim-web-devicons'},
      after = 'nvim-web-devicons',
    }

    use {
      'lewis6991/gitsigns.nvim',
      requires = 'nvim-lua/plenary.nvim',
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
    use { 'windwp/nvim-ts-autotag', requires = 'nvim-treesitter/nvim-treesitter' }
    use {
      'danymat/neogen',
      requires = 'nvim-treesitter/nvim-treesitter',
    }
    use 'mizlan/iswap.nvim'

    use {'dstein64/vim-startuptime', cmd = 'StartupTime',}

    use { 'karb94/neoscroll.nvim', config = [[require('neoscroll').setup()]] }

    use {
      'lukas-reineke/indent-blankline.nvim',
      after = 'kanagawa.nvim',
      config = function()
        local blankline = require('indent_blankline')
        blankline.setup {
          char_list = {'|', '¦', '┆', '┊'},
          filetype_exclude = {
            'help',
            'packer',
            'man',
            'lspinfo',
            'Trouble',
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
