function! statusline#git() abort
if exists('b:gitsigns_status')
  return b:gitsigns_status
end

if exists('*FugitiveHead')
  let branch = FugitiveHead()
  if branch !=# ''
    return branch . ' '
  endif
endif
return ''
endfunction

function! statusline#obsession() abort
if exists('*ObsessionStatus')
  let s = ObsessionStatus()
  if !empty(s)
    let s .= ' '
  endif
  return s
endif
return ''
endfunction

function! statusline#lsp() abort
if exists('b:lsp')
  return printf(' (%s/%d)', b:lsp.name, b:lsp.client_id)
endif
return ''
endfunction
