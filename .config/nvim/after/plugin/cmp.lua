local cmp = require 'cmp'

local lspkind = require 'lspkind'
local luasnip = require 'luasnip'
-- local neogen = require 'neogen'
-- local partial = require('jw.utils').partial
-- local copilot_comparators = require 'copilot_cmp.comparators'

-- local function cmp_pred(fn)
--   return cmp.mapping(function(fallback)
--     if cmp.visible() then
--       return fn()
--     end
--     return fallback()
--   end)
-- end

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },

  mapping = cmp.mapping.preset.insert {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),

    ['<C-Space>'] = cmp.mapping(cmp.mapping.complete {}, { 'i', 'c' }),
    ['<M-o>'] = cmp.mapping(cmp.mapping.complete {}, { 'i', 'c' }),

    ['<M-j>'] = cmp.mapping(function(fallback)
      if luasnip.jumpable(1) then
        luasnip.jump(1)
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),

    ['<M-k>'] = cmp.mapping(function(fallback)
      if luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),

    ['C-n'] = cmp.mapping.select_next_item {
      behavior = cmp.SelectBehavior.Insert,
    },

    ['C-p'] = cmp.mapping.select_prev_item {
      behavior = cmp.SelectBehavior.Insert,
    },

    ['<Tab>'] = cmp.config.disable,

    ['<C-k>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 'c' }),

    ['<C-j>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 'c' }),

    ['<C-q>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },

    ['<CR>'] = function(fallback)
      if cmp.visible() then
        cmp.confirm {
          behavior = cmp.ConfirmBehavior.Replace,
          select = false,
        }
      else
        fallback()
      end
    end,

    -- ['<Tab>'] = cmp.mapping(function(fallback)
    --   if cmp.visible() then
    --     cmp.select_next_item()
    --   elseif neogen.jumpable() then
    --     neogen.jump_next()
    --   else
    --     fallback()
    --   end
    -- end, { 'i', 's' }),
    --
    --   ['<S-Tab>'] = cmp.mapping(function(fallback)
    --     if cmp.visible() then
    --       cmp.select_prev_item()
    --     elseif neogen.jumpable(-1) then
    --       neogen.jump_prev()
    --     else
    --       fallback()
    --     end
    --   end, { 'i', 's' }),
  },

  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'copilot' },
    { name = 'nvim_lsp_signature_help' },
  }, { { name = 'luasnip' } }, {
    { name = 'path' },
    { name = 'buffer', keyword_length = 3 },
  }),

  formatting = {
    format = lspkind.cmp_format {
      mode = 'symbol_text',
      ellipsis_char = '...',
      maxwidth = 50,
      symbol_map = { Copilot = '' },
    },
  },

  sorting = {
    priority_weight = 2,
    comparators = {
      cmp.config.compare.offset,
      cmp.config.compare.exact,
      -- copilot_comparators.prioritize,
      -- copilot_comparators.score,
      cmp.config.compare.score,

      -- -- completion scores if the lsp supports it
      -- function(entry1, entry2)
      --   local weight1 = entry1.completion_item.score or 1
      --   local weight2 = entry2.completion_item.score or 1
      --   local score1 = entry1.score
      --   local score2 = entry2.score
      --   return (score2 * weight2) < (score1 * weight1)
      -- end,
      --
      -- shamelessly copied from https://github.com/lukas-reineke/cmp-under-comparator/blob/master/lua/cmp-under-comparator/init.lua
      -- since I cba dling a plugin for one function
      function(entry1, entry2)
        local _, entry1_under = entry1.completion_item.label:find '^_+'
        local _, entry2_under = entry2.completion_item.label:find '^_+'
        entry1_under = entry1_under or 0
        entry2_under = entry2_under or 0
        if entry1_under > entry2_under then
          return false
        elseif entry1_under < entry2_under then
          return true
        end
      end,

      cmp.config.compare.recently_used,
      cmp.config.compare.locality,
      cmp.config.compare.kind,
      cmp.config.compare.sort_text,
      cmp.config.compare.length,
      cmp.config.compare.order,
    },
  },

  -- confirm_opts = {
  --   behavior = cmp.ConfirmBehavior.Replace,
  --   select = false,
  -- },

  experimental = {
    native_menu = false,
    ghost_text = true,
  },
}
local ok, cmp_autopairs = pcall(require, 'nvim-autopairs.completion.cmp')
if ok then
  cmp.event:on(
    'confirm_done',
    cmp_autopairs.on_confirm_done {
      map_char = {
        tex = '',
      },
    }
  )
end

cmp.setup.filetype({ 'lua' }, {
  sources = cmp.config.sources({
    { name = 'nvim_lua' },
    { name = 'nvim_lsp' },
    { name = 'nvim_lsp_signature_help' },
    { name = 'copilot' },
  }, { { name = 'luasnip' } }, {
    { name = 'path' },
    { name = 'buffer', keyword_length = 3 },
  }),
})

cmp.setup.filetype({ 'orgmode' }, {
  sources = cmp.config.sources({
    { name = 'orgmode' },
  }, {
    { name = 'buffer', keyword_length = 3 },
  }, {
    { name = 'path' },
  }),
})

vim.cmd [[
augroup DadbodSql
  au!
  autocmd Filetype sql,mysql,plsql lua require('cmp').setup.buffer { sources = { { name = 'vim-dadbod-completion' } } }
augroup END
]]
vim.api.nvim_create_autocmd('BufRead', {
  group = vim.api.nvim_create_augroup('CmpSourceCargo', { clear = true }),
  pattern = 'Cargo.toml',
  callback = function()
    cmp.setup.buffer {
      sources = cmp.config.sources({
        { name = 'crates' },
        { name = 'nvim_lsp' },
        { name = 'copilot' },
      }, {
        { name = 'luasnip' },
        { name = 'path' },
        { name = 'buffer', keyword_length = 3 },
      }),
    }
  end,
})
local vscode_snips = require 'luasnip.loaders.from_vscode'
vscode_snips.lazy_load()
