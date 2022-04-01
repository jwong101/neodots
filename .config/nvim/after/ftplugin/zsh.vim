let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
setlocal textwidth=79
setlocal formatoptions-=t
let b:undo_ftplugin .= '|setl tw< fo<'
nnoremap <buffer> Z! ^"zg_:!<C-R>z<CR>


let b:undo_ftplugin .= '|nun <buffer> Z!'
