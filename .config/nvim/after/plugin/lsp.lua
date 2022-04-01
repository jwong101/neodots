local M = {}

local on_attach = function(_, bufnr)
  local api = vim.api
  local b = vim.bo
  local u = require('jw.utils')

  local function nset(lhs, rhs)
    vim.keymap.set('n', lhs, rhs, { buffer = bufnr })
  end

  b.omnifunc = 'v:lua.vim.lsp.omnifunc'
  b.tagfunc = 'v:lua.vim.lsp.tagfunc'

  b.formatexpr = 'v:lua.vim.lsp.formatexpr'

  nset('gd', '<cmd>lua vim.lsp.buf.declaration()<cr>')
  nset('K', '<cmd>lua vim.lsp.buf.hover()<cr>')
  nset('gD', '<cmd>lua vim.lsp.buf.implementation()<cr>')
  nset('<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<cr>')
  nset('<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<cr>')
  nset('<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<cr>')
  nset('<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<cr>')

  nset('<leader>ws', function()
    require('fzf-lua').lsp_workspace_symbols()
  end)

  nset('<leader>ca', function()
    require('fzf-lua').lsp_code_actions()
  end)

  nset('<leader>td', '<cmd>lua vim.lsp.buf.type_definition()<cr>')
  nset('<leader>rn', '<cmd>lua vim.lsp.buf.rename()<cr>')
  -- nsetk('<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<cr>')
  nset('<leader>gr', '<cmd>lua vim.lsp.buf.references()<cr>')

  local function cmd(lhs, rhs)
    return api.nvim_buf_add_user_command(bufnr, lhs, rhs, {})
  end

  local buflsp = vim.lsp.buf
  cmd('Format', u.lambda(buflsp.formatting_sync, {}, 1000))
  cmd('Symbols', buflsp.document_symbol)
  cmd('Calledby', buflsp.incoming_calls)
  cmd('Calling', buflsp.outgoing_calls)

  local ok,sig = pcall(require, "lsp_signature")
  if ok then
    sig.on_attach()
  end
end

function M.setup()
  local lsp_conf = require('lspconfig')
  local cmp_lsp = require 'cmp_nvim_lsp'
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = cmp_lsp.update_capabilities(capabilities)
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  local servers = { 'rust_analyzer', 'gopls', 'pyright', 'ocamllsp', 'graphql', 'vimls', 'yamlls' }
  vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics,
    {
      underline = true,
      virtual_text = {
        spacing = 5,
        severity_limit = 'Warning',
      },
      update_in_insert = true,
    }
  )

  vim.lsp.handlers['window/showMessage'] = function(_, result, ctx)
    local client = vim.lsp.get_client_by_id(ctx.client_id)
    local lvl = ({
      'ERROR',
      'WARN',
      'INFO',
      'DEBUG',
    })[result.type]

    vim.notify({ result.message }, lvl, {
      title = 'LSP | ' .. client.name,
      timeout = 10000,
      keep = function()
        return lvl == 'ERROR' or lvl == 'WARN'
      end,
    })
  end

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
    flags = {
      debounce_text_changes = 20,
    },
    init_options = {
      clangdFileStatus = true,
    },
    on_attach = on_attach,
    capabilities = capabilities,
  }

  lsp_conf.jsonls.setup {
    on_attach = on_attach,
    capabilities = capabilities,
    filetypes = { "json", "jsonc" },
    init_options = {
      provideFormatter = true,
    },
    single_file_support = true,
    settings = {
      json = {
        schemas = require("schemastore").json.schemas(),
      },
    },
  }

  lsp_conf['hls'].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
      haskell = {
        formattingProvider = "stylish-haskell",
      },
    },
  }

  lsp_conf['denols'].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    root_dir = lsp_conf.util.root_pattern("deno.json"),
    init_options = {
      enable = true,
      lint = true,
      unstable = true,
    },
  }

  lsp_conf["graphql"].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    root_dir = lsp_conf.util.root_pattern(".graphqlrc", ".graphqlrc.yaml", ".graphqlrc.yml", ".graphqlrc.config.yaml", ".graphqlrc.json", "graphqlrc.config.json", "graphqlrc.config.js", ".graphqlrc.js"),
  }

  lsp_conf.tsserver.setup {
    on_attach = function(client, bufnr)
      local ts_utils = require('nvim-lsp-ts-utils')
      ts_utils.setup {
        enable_imports_on_completion = true,
        filter_out_diagnostics_by_code = {
          8001,
        },
      }
      ts_utils.setup_client(client)
      on_attach(client, bufnr)
    end,
    capabilities = capabilities,
    root_dir = lsp_conf.util.root_pattern("package.json"),
    flags = {
      debounce_text_changes = 150,
    },
  }

  lsp_conf.eslint.setup {
    root_dir = lsp_conf.util.root_pattern(".eslintrc", ".eslintrc.js"),
    on_attach = function (client, bufnr)
      client.resolved_capabilities.document_formatting = true
      on_attach(client, bufnr)
    end,
    capabilities = capabilities,
    settings = {
      packageManager = "pnpm",
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

