let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
setlocal textwidth=88
setlocal formatoptions-=t
setlocal foldmethod=indent
setlocal foldnestmax=2

let b:undo_ftplugin .= '|setl tw< fo< fdm< fdn<'

let &l:define='^\ze\i\+\s*=\|\<def\>'

if exists('$VIRTUAL_ENV') && filereadable($VIRTUAL_ENV . '/tags')
    let &l:tags = $VIRTUAL_ENV . '/tags,' . &tags
endif

if executable('black')
    setlocal formatprg=black\ -q\ -\ 2>/dev/null
endif

if executable('isort')
    if empty(&l:formatprg)
        setlocal formatprg=isort\ -q\ -
    else
        setlocal formatprg+=\ \|\ isort\ -q\ -
    endif
endif

if !empty(&l:formatprg)
    let b:undo_ftplugin .= '|setl fp<'
endif
