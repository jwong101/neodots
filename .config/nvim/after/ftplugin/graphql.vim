let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
setlocal softtabstop=2
setlocal shiftwidth=2
let b:undo_ftplugin .= '|setl sts< sw<'
