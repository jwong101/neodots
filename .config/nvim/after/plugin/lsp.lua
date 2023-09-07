local M = {}

local on_attach = function(client, bufnr)
  local b = vim.bo
  local u = require 'jw.utils'

  local function nset(lhs, rhs)
    vim.keymap.set('n', lhs, rhs, { silent = true, buffer = bufnr })
  end

  if client.server_capabilities.completionProvider then
    b[bufnr].omnifunc = 'v:lua.vim.lsp.omnifunc'
  end
  if client.server_capabilities.definitionProvider then
    b[bufnr].tagfunc = 'v:lua.vim.lsp.tagfunc'
  end

  b.formatexpr = 'v:lua.vim.lsp.formatexpr'
  local api = vim.api
  if client.server_capabilities.documentHighlightProvider then
    local group_id = vim.api.nvim_create_augroup('lsp_document_highlight', {})
    vim.api.nvim_create_autocmd('CursorHold', {
      group = group_id,
      buffer = bufnr,
      callback = vim.lsp.buf.document_highlight,
    })
    vim.api.nvim_create_autocmd('CursorHoldI', {
      group = group_id,
      buffer = bufnr,
      callback = vim.lsp.buf.document_highlight,
    })
    vim.api.nvim_create_autocmd('CursorMoved', {
      group = group_id,
      buffer = bufnr,
      callback = vim.lsp.buf.clear_references,
    })
  end
  if client.server_capabilities.codeActionProvider then
    local bulb_id = api.nvim_create_augroup('lightbulb', {})
    local update_lightbulb = require('nvim-lightbulb').update_lightbulb
    local update_lightbulb_cb = function(_)
      update_lightbulb()
    end
    vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
      group = bulb_id,
      buffer = bufnr,
      callback = update_lightbulb_cb,
    })
  end

  if client.server_capabilities.codeLensProvider then
    local lens = api.nvim_create_augroup('codelens', {})
    local update_lens_cb = function(_)
      vim.lsp.codelens.refresh()
    end
    vim.api.nvim_create_autocmd('BufEnter', {
      group = lens,
      buffer = bufnr,
      callback = function(_)
        vim.lsp.codelens.refresh()
        return true
      end,
    })
    vim.api.nvim_create_autocmd({ 'CursorHold', 'InsertLeave' }, {
      group = lens,
      buffer = bufnr,
      callback = update_lens_cb,
    })
    nset('<leader>cl', '<cmd>lua vim.lsp.codelens.run()<cr>')
  end

  nset('gd', '<cmd>lua vim.lsp.buf.declaration()<cr>')
  nset('K', '<cmd>lua vim.lsp.buf.hover()<cr>')
  vim.keymap.set('i', '<C-s>', vim.lsp.buf.signature_help, { buffer = true })
  local ft = vim.api.nvim_buf_get_option(bufnr, 'filetype')
  if ft == 'rust' then
    -- remove #[derive(...)] results, since they are redundant with `go to type_definition`
    local filter_derives = function(opts)
      local new_items = vim.tbl_filter(function(item)
        return not string.find(item.text, '#[derive(', 1, true)
      end, opts.items)
      if #new_items > 0 then
        vim.fn.setqflist({}, ' ', { items = new_items, title = opts.title })
        local jmp = #new_items > 1 and 'copen | cfirst' or 'cfirst'
        vim.cmd(jmp)
      end
    end
    local no_derive_impl = function()
      vim.lsp.buf.implementation { on_list = filter_derives }
    end
    nset('<leader>ii', no_derive_impl)
    nset('gm', no_derive_impl)
  else
    nset('<leader>ii', '<cmd>lua vim.lsp.buf.implementation()<cr>')
    nset('gm', '<cmd>Telescope lsp_implementations<cr>')
  end
  nset('gM', '<cmd>Glance implementations<cr>')
  nset('gR', '<cmd>Glance references<cr>')
  nset('gr', '<cmd>lua vim.lsp.buf.references()<cr>')
  nset('gY', '<cmd>Glance type_definitions<cr>')
  nset('gy', '<cmd>Telescope lsp_type_definitions<cr>')
  nset('gD', '<cmd>Glance definitions<cr>')
  nset('<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<cr>')
  nset('<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<cr>')
  nset(
    '<leader>wl',
    '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<cr>'
  )

  nset('<leader>it', '<cmd>lua vim.lsp.buf.type_definition()<cr>')
  nset('<leader>rn', '<cmd>lua vim.lsp.buf.rename()<cr>')
  -- nsetk('<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<cr>')
  nset('<leader>ir', '<cmd>Telescope lsp_references<cr>')

  local cmd = u.buf_add_cmd(bufnr)
  local leader = u.leader

  leader(
    'ws',
    '<cmd>Telescope lsp_workspace_symbols<cr>',
    { silent = true, buffer = bufnr }
  )
  leader(
    'bs',
    '<cmd>Telescope lsp_document_symbols<cr>',
    { silent = true, buffer = bufnr }
  )
  leader(
    'ca',
    '<cmd>lua vim.lsp.buf.code_action()<CR>',
    { buffer = bufnr, silent = true }
  )
  leader(
    'lf',
    '<cmd>lua vim.lsp.buf.formatting_sync()<CR>',
    { buffer = bufnr, silent = true }
  )
  leader(
    'io',
    '<cmd>lua vim.lsp.buf.incoming_calls()<CR>',
    { buffer = bufnr, silent = true }
  )
  leader(
    'iO',
    '<cmd>lua vim.lsp.buf.outgoing_calls()<CR>',
    { buffer = bufnr, silent = true }
  )
  leader(
    'is',
    '<cmd>lua vim.lsp.buf.document_symbol()<CR>',
    { buffer = bufnr, silent = true }
  )
  cmd('Format', '<cmd>lua vim.lsp.buf.formatting_sync()<CR>', {})
  cmd('Symbols', '<cmd>lua vim.lsp.buf.document_symbol()<CR>', {})
  cmd('Calledby', '<cmd>lua vim.lsp.buf.incoming_calls()<CR>', {})
  cmd('Calling', '<cmd>lua vim.lsp.buf.outgoing_calls()<CR>', {})

  --[[ local ok, sig = pcall(require, 'lsp_signature') ]]
  --[[ if ok then ]]
  --[[   sig.on_attach() ]]
  --[[ end ]]
end

function M.setup()
  local lsp_conf = require 'lspconfig'
  local cmp_lsp = require 'cmp_nvim_lsp'
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities =
    vim.tbl_deep_extend('force', capabilities, cmp_lsp.default_capabilities())
  capabilities.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true,
  }
  local servers = {
    'awk_ls',
    'bashls',
    'cssls',
    'dockerls',
    'dotls',
    'gopls',
    'graphql',
    'html',
    'ocamllsp',
    'taplo',
    'vimls',
    'yamlls',
  }
  vim.lsp.handlers['textDocument/publishDiagnostics'] =
    vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      underline = true,
      virtual_text = {
        spacing = 5,
        severity_limit = 'Warning',
      },
      update_in_insert = false,
    })

  --[[ vim.lsp.handlers['window/showMessage'] = function(_, result, ctx) ]]
  --[[   local client = vim.lsp.get_client_by_id(ctx.client_id) ]]
  --[[   local lvl = ({ ]]
  --[[     'ERROR', ]]
  --[[     'WARN', ]]
  --[[     'INFO', ]]
  --[[     'DEBUG', ]]
  --[[   })[result.type] ]]
  --[[]]
  --[[   vim.notify_once({ result.message }, lvl, { ]]
  --[[     title = 'LSP | ' .. client.name, ]]
  --[[     timeout = 10000, ]]
  --[[     keep = function() ]]
  --[[       return lvl == 'ERROR' or lvl == 'WARN' ]]
  --[[     end, ]]
  --[[   }) ]]
  --[[ end ]]

  for _, lsp in ipairs(servers) do
    lsp_conf[lsp].setup {
      on_attach = on_attach,
      capabilities = capabilities,
    }
  end

  lsp_conf['pyright'].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
      python = {
        analysis = {
          useLibraryCodeForTypes = true,
          diagnosticMode = 'openFilesOnly',
          typeCheckingMode = 'basic',
        },
      },
    },
  }

  lsp_conf['clangd'].setup {
    cmd = {
      'clangd',
      '--background-index',
      '--suggest-missing-includes',
      '--clang-tidy',
      '--header-insertion=iwyu',
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

  lsp_conf.neocmake.setup {
    on_attach = on_attach,
    capabilities = capabilities,
    root_dir = lsp_conf.util.root_pattern(
      'CMakePresets.json',
      'CTestConfig.cmake',
      '.git',
      'build',
      'cmake'
    ),
    single_file_support = true,
  }

  lsp_conf.jsonls.setup {
    on_attach = on_attach,
    capabilities = capabilities,
    filetypes = { 'json', 'jsonc' },
    init_options = {
      provideFormatter = true,
    },
    single_file_support = true,
    settings = {
      json = {
        schemas = require('schemastore').json.schemas(),
      },
    },
  }

  lsp_conf['hls'].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
      haskell = {
        formattingProvider = 'stylish-haskell',
      },
    },
  }

  lsp_conf['denols'].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    root_dir = lsp_conf.util.root_pattern 'deno.json',
    init_options = {
      enable = true,
      lint = true,
      unstable = true,
    },
    single_file_support = false,
  }

  lsp_conf['graphql'].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    root_dir = lsp_conf.util.root_pattern(
      '.graphqlrc',
      '.graphqlrc.yaml',
      '.graphqlrc.yml',
      '.graphqlrc.config.yaml',
      '.graphqlrc.json',
      'graphqlrc.config.json',
      'graphqlrc.config.js',
      '.graphqlrc.js'
    ),
  }

  lsp_conf.tsserver.setup {
    on_attach = function(client, bufnr)
      local ts_utils = require 'nvim-lsp-ts-utils'
      ts_utils.setup {
        enable_imports_on_completion = true,
        filter_out_diagnostics_by_code = {
          8001,
        },
        update_imports_on_move = true,
      }
      ts_utils.setup_client(client)
      on_attach(client, bufnr)
    end,
    capabilities = capabilities,
    root_dir = lsp_conf.util.root_pattern 'package.json',
    flags = {
      debounce_text_changes = 150,
    },
  }

  lsp_conf.eslint.setup {
    root_dir = lsp_conf.util.root_pattern('.eslintrc', '.eslintrc.js'),
    on_attach = function(client, bufnr)
      client.server_capabilities.document_formatting = true
      on_attach(client, bufnr)
    end,
    capabilities = capabilities,
    settings = {
      packageManager = 'pnpm',
    },
  }

  local runtime_path = vim.split(package.path, ';')
  table.insert(runtime_path, 'lua/?.lua')
  table.insert(runtime_path, 'lua/?/init.lua')
  lsp_conf.lua_ls.setup {
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
  local extension_path = vim.env.XDG_CONFIG_HOME
    .. '.local/share/vscode-extensions/codelldb/extensions'
  local codelldb_path = extension_path .. 'adapter/codelldb'
  local liblldb_path = extension_path .. 'lldb/lib/liblldb.so'
  --[[ local impl_handle = vim.lsp.handlers['textDocument/implementation'] ]]
  require('rust-tools').setup {
    server = {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        ['rust-analyzer'] = {
          checkOnSave = {
            command = 'clippy',
          },
        },
      },
    },
    dap = {
      adapter = require('rust-tools.dap').get_codelldb_adapter(
        codelldb_path,
        liblldb_path
      ),
    },
  }
end

M.setup()
