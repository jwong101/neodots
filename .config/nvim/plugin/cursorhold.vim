" Maintainer:   Antoine Madec <aja.madec@gmail.com>

if exists('g:loaded_fix_cursorhold_nvim')
  finish
endif
let g:loaded_fix_cursorhold_nvim = 1

let g:cursorhold_updatetime = 250
let g:fix_cursorhold_nvim_timer = -1
set eventignore+=CursorHold,CursorHoldI

augroup fix_cursorhold_nvim
  autocmd!
  autocmd CursorMoved * call cursorhold#CursorHoldTimer()
  autocmd CursorMovedI * call cursorhold#CursorHoldITimer()
augroup end

