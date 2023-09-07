local ok, lualine = pcall(require, 'lualine')
if not ok then
  return
end

local current_signature = {
  function()
    local buf_ft = vim.bo.filetype
    if buf_ft == 'TelescopePrompt' then
      return ''
    end
    local sig_ok, signature = pcall(require, 'lsp_signature')
    if not sig_ok then
      return ''
    end
    return signature.status_line(30).label
  end,
  cond = function()
    return vim.o.columns > 100
  end,
  padding = 0,
}

lualine.setup {
  options = {
    globalStatus = true,
  },
  sections = {
    lualine_a = { 'mode' },
    lualine_b = { 'branch', 'diff', 'diagnostics' },
    lualine_c = { current_signature, 'filename' },
    lualine_x = { 'fileformat', 'filetype' },
    lualine_y = { 'progress' },
    lualine_z = { 'location' },
  },
  extensions = { 'quickfix', 'fugitive', 'man', 'nvim-dap-ui' },
}
