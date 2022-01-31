let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal textwidth=72
setlocal formatoptions+=cn
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:

let b:undo_ftplugin .= '|setl tw< fo< flp<'
match ErrorMsg /\%1l.\%>51v/
