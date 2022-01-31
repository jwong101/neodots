local M = {}

local on_attach = function(client, bufnr)
  local opts = { noremap=true, silent=true }
  local api = vim.api
  local b = vim.bo

  local function nnore(key, action) api.nvim_buf_set_keymap(bufnr, 'n', key, action, opts) end

  local function nsetk(lhs, rhs)
    vim.keymap.set('n', lhs, rhs, { buffer = bufnr })
  end

  b.omnifunc = 'v:lua.vim.lsp.omnifunc'
  b.tagfunc = 'v:lua.vim.lsp.tagfunc'
  b.formatexpr = 'v:lua.vim.lsp.formatexpr'

  nnore('gd', '<cmd>lua vim.lsp.buf.declaration()<cr>')
  nnore('K', '<cmd>lua vim.lsp.buf.hover()<cr>')
  nnore('gD', '<cmd>lua vim.lsp.buf.implementation()<cr>')
  nnore('<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<cr>')
  nnore('<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<cr>')
  nnore('<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<cr>')
  nnore('<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<cr>')

  nsetk('<leader>ws', function()
    require('fzf-lua').lsp_workspace_symbols()
  end)

  nsetk('<leader>ca', function()
    require('fzf-lua').lsp_code_actions()
  end)

  nnore('<leader>td', '<cmd>lua vim.lsp.buf.type_definition()<cr>')
  nnore('<leader>rn', '<cmd>lua vim.lsp.buf.rename()<cr>')
  -- nnore('<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<cr>')
  nnore('<leader>gr', '<cmd>lua vim.lsp.buf.references()<cr>')

  local function cmd(lhs, rhs)
    return api.nvim_buf_add_user_command(bufnr, lhs, rhs, {})
  end

  local buflsp = vim.lsp.buf
  cmd('Format', buflsp.formatting)
  cmd('Symbols', buflsp.document_symbol)
  cmd('Calledby', buflsp.incoming_calls)
  cmd('Calling', buflsp.outgoing_calls)
end

function M.setup()
  local lsp_conf = require('lspconfig')
  local cmp_lsp = require 'cmp_nvim_lsp'
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = cmp_lsp.update_capabilities(capabilities)
  local servers = { 'rust_analyzer', 'gopls', 'pyright', 'hls' }

  for _, lsp in ipairs(servers) do
    lsp_conf[lsp].setup {
      on_attach = on_attach,
      capabilities = capabilities,
    }
  end

  lsp_conf['clangd'].setup {
    cmd = {
      "clangd",
      "--background-index",
      "--suggest-missing-includes",
      "--clang-tidy",
      "--header-insertion=iwyu",
    },
    init_options = {
      clangdFileStatus = true,
    },
    on_attach = on_attach,
    capabilities = capabilities,
  }

  lsp_conf['denols'].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    init_options = {
      enable = true,
      lint = true,
      unstable = true,
    },
  }

  local runtime_path = vim.split(package.path, ';')
  table.insert(runtime_path, 'lua/?.lua')
  table.insert(runtime_path, 'lua/?/init.lua')
  lsp_conf.sumneko_lua.setup {
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
      Lua = {
        runtime = {
          version = 'LuaJIT',
          path = runtime_path,
        },
        diagnostics = {
          globals = { 'vim' },
        },
        workspace = {
          library = vim.api.nvim_get_runtime_file('', true),
        },
        telemetry = {
          enable = false,
        },
      },
    },
  }
end

M.setup()

