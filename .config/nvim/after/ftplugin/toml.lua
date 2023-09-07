if vim.fn.expand '%:t' ~= 'Cargo.toml' then
  return
end
local ok, crates = pcall(require, 'crates')
if not ok then
  return
end
local b = vim.b
local bo = vim.bo
if crates.popup_available() then
  bo.keywordprg = [[:lua require('crates').show_popup()]]
end

b.undo_ftplugin = b.undo_ftplugin or ''
b.undo_ftplugin = b.undo_ftplugin .. '|setl kp<'
