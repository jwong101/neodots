
function! CursorHold_Cb(timer_id) abort
  set eventignore-=CursorHold
  doautocmd <nomodeline> CursorHold
  set eventignore+=CursorHold
endfunction

function! CursorHoldI_Cb(timer_id) abort
  set eventignore-=CursorHoldI
  doautocmd <nomodeline> CursorHoldI
  set eventignore+=CursorHoldI
endfunction

function! cursorhold#CursorHoldTimer() abort
  call timer_stop(g:fix_cursorhold_nvim_timer)
  if mode() == 'n'
    let g:fix_cursorhold_nvim_timer = timer_start(g:cursorhold_updatetime, 'CursorHold_Cb')
  endif
endfunction

function! cursorhold#CursorHoldITimer() abort
  call timer_stop(g:fix_cursorhold_nvim_timer)
  let g:fix_cursorhold_nvim_timer = timer_start(g:cursorhold_updatetime, 'CursorHoldI_Cb')
endfunction
