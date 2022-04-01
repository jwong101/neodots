local trouble = require('trouble')

trouble.setup {
  auto_close = true,
}

local u = require('jw.utils')

u.nmap('<leader>xx', '<cmd>TroubleToggle<cr>')
u.nmap('<leader>xw', '<cmd>TroubleToggle workspace_diagnostics<cr>')
u.nmap('<leader>xd', '<cmd>TroubleToggle document_diagnostics<cr>')
u.nmap('<leader>xl', '<cmd>TroubleToggle loclist<cr>')
u.nmap('<leader>xq', '<cmd>TroubleToggle quickfix<cr>')
u.nmap('<leader>xR', '<cmd>TroubleToggle lsp_references<cr>')
u.nmap('[x', u.partial(trouble.previous, { skip_groups = true, jump = true }))
u.nmap(']x', u.partial(trouble.next, { skip_groups = true, jump = true }))
