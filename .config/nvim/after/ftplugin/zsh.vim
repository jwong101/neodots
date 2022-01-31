nnoremap <buffer> Z! ^"zg_:!<C-R>z<CR>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|nun <buffer> Z!'
