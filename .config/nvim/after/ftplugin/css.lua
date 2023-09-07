local bo = vim.bo
local b = vim.b
bo.shiftwidth = 2
bo.softtabstop = 2
b.undo_ftplugin = b.undo_ftplugin or '|setl sw< sts<'
