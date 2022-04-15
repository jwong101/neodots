local M = {}

local on_attach = function(_, bufnr)
  local b = vim.bo
  local u = require('jw.utils')

  local function nset(lhs, rhs)
    vim.keymap.set('n', lhs, rhs, { silent = true, buffer = bufnr })
  end

  b.omnifunc = 'v:lua.vim.lsp.omnifunc'
  b.tagfunc = 'v:lua.vim.lsp.tagfunc'

  b.formatexpr = 'v:lua.vim.lsp.formatexpr'

  nset('gd', '<cmd>lua vim.lsp.buf.declaration()<cr>')
  nset('K', '<cmd>lua vim.lsp.buf.hover()<cr>')
  nset('gD', '<cmd>lua vim.lsp.buf.implementation()<cr>')
  nset('<M-k>', '<cmd>lua vim.lsp.buf.signature_help()<cr>')
  nset('<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<cr>')
  nset('<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<cr>')
  nset('<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<cr>')



  nset('<leader>it', '<cmd>lua vim.lsp.buf.type_definition()<cr>')
  nset('<leader>rn', '<cmd>lua vim.lsp.buf.rename()<cr>')
  -- nsetk('<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<cr>')
  nset('<leader>ir', '<cmd>Telescope lsp_references<cr>')

  local cmd = u.buf_add_cmd(bufnr)
  local leader = u.leader

  leader('ws', '<cmd>Telescope lsp_workspace_symbols<cr>', {silent = true, buffer = bufnr})
  leader('bs', '<cmd>Telescope lsp_document_symbols<cr>', {silent = true, buffer = bufnr})
  leader('ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', {buffer = bufnr, silent = true})
  leader('lf', '<cmd>lua vim.lsp.buf.formatting_sync()<CR>', {buffer = bufnr, silent = true})
  leader('ii', '<cmd>lua vim.lsp.buf.incoming_calls()<CR>', {buffer = bufnr, silent = true})
  leader('io', '<cmd>lua vim.lsp.buf.outgoing_calls()<CR>', {buffer = bufnr, silent = true})
  leader('is', '<cmd>lua vim.lsp.buf.document_symbol()<CR>', {buffer = bufnr, silent = true})
  cmd('Format', '<cmd>lua vim.lsp.buf.formatting_sync()<CR>', {})
  cmd('Symbols', '<cmd>lua vim.lsp.buf.document_symbol()<CR>', {})
  cmd('Calledby', '<cmd>lua vim.lsp.buf.incoming_calls()<CR>', {})
  cmd('Calling', '<cmd>lua vim.lsp.buf.outgoing_calls()<CR>', {})

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
  local servers = { 'dockerls', 'rust_analyzer', 'gopls', 'pyright', 'ocamllsp', 'graphql', 'vimls', 'yamlls' }
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

