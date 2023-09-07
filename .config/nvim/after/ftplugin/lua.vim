let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
setlocal softtabstop=2
setlocal shiftwidth=2
let b:undo_ftplugin .= '|setl sts< sw<'

setlocal formatprg=stylua\ -
" augroup StyluaFormat
"     autocmd!
"     autocmd BufWritePre <buffer> keepjumps normal m'gggqG``
" augroup END
let b:undo_ftplugin .= '|setl fp<'
