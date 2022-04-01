let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal noexpandtab
setlocal softtabstop=8
setlocal shiftwidth=8
let b:undo_ftplugin .= '|setl et< sts< sw<'

let &l:define='\<func\>|\<const\>|^\s*\ze\i\+\s*:='
let b:undo_ftplugin .= '|setl def<'

if executable('gofmt')
    setlocal formatprg=gofmt
endif

if !empty(&l:formatprg)
    let b:undo_ftplugin .= '|setl fp<'
endif
