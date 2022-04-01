local cmp = require 'cmp'

local lspkind = require 'lspkind'
local luasnip = require 'luasnip'
local neogen = require 'neogen'
local partial = require('jw.utils').partial

local function cmp_pred(fn)
  return cmp.mapping(function(fallback)
    if cmp.visible() then
      return fn()
    end
    return fallback()
  end)
end
cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },

  mapping = {
    ['<C-b>'] = cmp_pred(partial(cmp.mapping.scroll_docs, -4)),
    ['<C-f>'] = cmp_pred(partial(cmp.mapping.scroll_docs, 4)),

    ['<C-y>'] = cmp.mapping(
      cmp.mapping.confirm {
        behavior = cmp.ConfirmBehavior.Insert,
        select = true,
      },
      { 'i' }
    ),

    ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i' }),
    -- ['<C-Space>'] = cmp.mapping({
    --   i = cmp.mapping.complete(),
    --   c = function()
    --     if cmp.visible() then
    --       cmp.complete()
    --       return
    --     end
    --     cmp.confirm { select = true }
    --   end
    -- }),

    ['<C-e>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.mapping.abort()
      else
        fallback()
      end
    end),

    ['<C-q>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<CR>'] = cmp.mapping.confirm({ select = true }),

    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif neogen.jumpable() then
        neogen.jump_next()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { "i", "s" }),

    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif neogen.jumpable(-1) then
        neogen.jump_prev()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),
  },

  sources = {
    { name = 'nvim_lua' },
    { name = 'nvim_lsp' },
    { name = 'path' },
    { name = 'luasnip' },
    { name = 'buffer', keyword_length = 5 },
  },

  formatting = {
    format = lspkind.cmp_format {
      with_text = true,
      maxwidth = 50,
      menu = {
        buffer = "[buf]",
        nvim_lsp = "[LSP]",
        path = "[path]",
        nvim_lua = "[api]",
        luasnip = "[snip]",
      },
    },
  },

  sorting = {
    comparators = {
      cmp.config.compare.offset,
      cmp.config.compare.exact,
      cmp.config.recently_used,

      -- completion scores if the lsp supports it
      function (entry1, entry2)
        local weight1 = entry1.completion_item.score or 1
        local weight2 = entry2.completion_item.score or 1
        local score1 = entry1.score
        local score2 = entry2.score
        return (score2 * weight2) < (score1 * weight1)
      end,

      -- shamelessly copied from https://github.com/lukas-reineke/cmp-under-comparator/blob/master/lua/cmp-under-comparator/init.lua
      -- since I cba dling a plugin for one function
      function(entry1, entry2)
        local _, entry1_under = entry1.completion_item.label:find "^_+"
        local _, entry2_under = entry2.completion_item.label:find "^_+"
        entry1_under = entry1_under or 0
        entry2_under = entry2_under or 0
        if entry1_under > entry2_under then
          return false
        elseif entry1_under < entry2_under then
          return true
        end
      end,

      cmp.config.compare.kind,
      cmp.config.compare.sort_text,
      cmp.config.compare.length,
      cmp.config.compare.order,
    },
  },

  experimental = {
    native_menu = false,
  },
}
local ok, cmp_autopairs = pcall(require, 'nvim-autopairs.completion.cmp')
if ok then
  cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done {
    map_char = {
      tex = '',
    },
  })
end

vim.cmd [[
augroup DadbodSql
  au!
  autocmd Filetype sql,mysql,plsql lua require('cmp').setup.buffer { sources = { { name = 'vim-dadbod-completion' } } }
augroup END
]]

local vscode_snips = require("luasnip.loaders.from_vscode")
vscode_snips.lazy_load()
