local diagnostic = vim.diagnostic
local u = require('jw.utils')
u.nmap('[g', diagnostic.goto_prev)
u.nmap(']g', diagnostic.goto_next)
u.nmap('<leader>gs', diagnostic.open_float)
u.nmap('<leader>gl', diagnostic.setloclist)
u.nmap('<leader>gq', diagnostic.setqflist)
