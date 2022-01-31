local trouble = require('trouble')

trouble.setup {
  auto_open = true,
  use_diagnostic_signs = true,
}

local nsetk = function(lhs, rhs)
  vim.keymap.set('n', lhs, rhs, {silent = true})
end

nsetk('<leader>xx', '<cmd>Trouble<cr>')
nsetk('<leader>xw', '<cmd>Trouble workspace_diagnostics<cr>')
nsetk('<leader>xd', '<cmd>Trouble document_diagnostics<cr>')
nsetk('<leader>xl', '<cmd>Trouble loclist<cr>')
nsetk('<leader>xq', '<cmd>Trouble quickfix<cr>')
nsetk('[x', function() trouble.previous({ skip_groups = true, jump = true }) end)
nsetk(']x', function() trouble.next({ skip_groups = true, jump = true }) end)
