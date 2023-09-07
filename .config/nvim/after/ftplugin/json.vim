setlocal formatprg=jq
let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<'

setlocal shiftwidth=2
setlocal softtabstop=2
let b:undo_ftplugin .= '|setl sw< sts<'
