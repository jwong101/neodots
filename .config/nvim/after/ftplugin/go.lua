local b = vim.bo
b.expandtab = false
b.softtabstop = 8
b.shiftwidth = 8
b.formatprg = 'gofmt'
b.define = [[\<func\>\|\<const\>\|^\s*\ze\i\+\s*:=]]
