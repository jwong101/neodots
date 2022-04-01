let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
let b:current_compiler = get(b:, 'current_compiler', '')
if !b:current_compiler
    if findfile('Cargo.toml', '.;')->empty() == 0
        compiler cargo
    else
        compiler rustc
    endif
endif

setlocal formatprg=rustfmt\ -q\ --emit=stdout
let &l:include = '\\v^\\s*(pub\\s+)?use\\s+\\zs(\\f\|:)+'
let b:undo_ftplugin .= '|setl fp< inc<'
