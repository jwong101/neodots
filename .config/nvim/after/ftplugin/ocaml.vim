let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
setlocal textwidth=80
let b:undo_ftplugin .= '|setl tw<'
setlocal softtabstop=2
setlocal shiftwidth=2
let b:undo_ftplugin .= '|setl sts< sw<'
