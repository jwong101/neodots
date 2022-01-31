let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

nnoremap <silent> <buffer> q <Cmd>q<CR>
let b:undo_ftplugin .= '|nun <buffer> q'

nmap <silent> <buffer> { <Plug>(qf_previous_file)
nmap <silent> <buffer> } <Plug>(qf_next_file)
let b:undo_ftplugin .= '|nun <buffer> {' . '|nun <buffer> }'

augroup qf
    autocmd!
    autocmd BufEnter <buffer> ++nested if winnr('$') < 2 | q | endif
augroup END
let b:undo_ftplugin .= '|au! qf * <buffer>'

if !exists('loaded_cfilter')
    silent! packadd cfilter
    let loaded_cfilter = get(g:, 'loaded_cfilter')
endif
