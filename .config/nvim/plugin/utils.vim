if exists('g:loaded_utils')
  finish
endif
let g:loaded_utils = 1

command! -nargs=1 -complete=command -bar -range Redir silent call utils#Redir(<q-args>, <range>, <line1>, <line2>)
