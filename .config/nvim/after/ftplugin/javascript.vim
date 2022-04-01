let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal textwidth=80
let b:undo_ftplugin .= '|setl tw<'

setlocal shiftwidth=2
setlocal softtabstop=2
let b:undo_ftplugin .= '|setl sw< sts<'
