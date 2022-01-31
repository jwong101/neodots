local current_compiler = vim.b.current_compiler
if not current_compiler or current_compiler == '' then
  if vim.fn.findfile('Cargo.toml', '.;') ~= "" then
    vim.cmd 'compiler cargo'
  else
    vim.cmd 'compiler rustc'
  end
end


vim.bo.formatprg = [[rustfmt -q --emit=stdout]]
vim.bo.include = [[\\v^\\s*(pub\\s+)?use\\s+\\zs(\\f\|:)+]]
-- vim.bo.define = [[^\s*fn\s\+]]


