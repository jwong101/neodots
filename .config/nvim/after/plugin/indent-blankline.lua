if true then
  return
end

local loaded, ibl = pcall(require, 'indent_blankline')

if not loaded then
  return
end

local g = vim.g
g.indent_blankline_char_list = {'|', '¦', '┆', '┊'}
g.indent_blankline_buftype_exclude = { 'terminal', 'nofile', }
g.indent_blankline_filetype_exclude = { 'help', 'packer', 'man', }

ibl.setup {
  space_char_blankline = " ",
  show_current_context = true,
  show_current_context_start = true,
  show_end_of_line = true,
}
