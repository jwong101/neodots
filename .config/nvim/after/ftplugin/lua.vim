let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
setlocal softtabstop=2
setlocal shiftwidth=2
let b:undo_ftplugin .= '|setl sts< sw<'

if findfile('stylua.toml', '.;')->empty() == 0
    setlocal formatprg=stylua\ -
    echom "test"
    let b:undo_ftplugin .= '|setl fp<'
endif
