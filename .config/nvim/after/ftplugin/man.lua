local function km(lhs, rhs, opt)
  opt = opt or {}
  local default = { silent = true, buffer = true }
  default = vim.tbl_extend('force', default, opt)
  vim.keymap.set('n', lhs, rhs, default)
end


km('<localleader>f', '/^\\s\\+-')
km('d', '<C-D>', { nowait = true })
km('u', '<C-U>')
km('<Tab>', "/\\C\\%>1l\\f\\+([1-9][a-z]\\=)\\ze\\_.\\+\\%$<CR><Cmd>nohlsearch<CR>")
km('<S-Tab>', "?\\C\\%>1l\\f\\+([1-9][a-z]\\=)\\ze\\_.\\+\\%$<CR><Cmd>nohlsearch<CR>")
km('<CR>', '<C-]')
km('<BS>', '<C-T')

local name = string.match(vim.api.nvim_buf_get_name(0), "man://(.+)")
vim.wo.statusline = string.format(" %s%%=%%14.(%%l:%%c%%V%%)%%14.P ", name)


local ftp = vim.b.undo_ftplugin
local keys = {'<localleader>f', 'd', 'u', '<Tab>', '<S-Tab', '<CR>', '<BS>'}
for _, key in ipairs(keys) do
  ftp = ftp .. string.format("|nun <buffer> %s", key)
end

ftp = ftp .. "|setl stl<"
