nnoremap <buffer> <CR> g<C-]>
nnoremap <silent> <buffer> <C-J> /<Bar>\S\{-\}<Bar><CR><Cmd>nohlsearch<CR>
nnoremap <silent> <buffer> <C-K> /<Bar>\S\{-\}<Bar><CR><Cmd>nohlsearch<CR>

nnoremap <nowait> <buffer> q <C-W>q
nnoremap <nowait> <buffer> d <C-d>
nnoremap <nowait> <buffer> u <C-u>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
    \ . '|unm <buffer> <CR>'
    \ . '|unm <buffer> <C-J>'
    \ . '|unm <buffer> <C-K>'
    \ . '|unm <buffer> q'
    \ . '|unm <buffer> d'
    \ . '|unm <buffer> u'
