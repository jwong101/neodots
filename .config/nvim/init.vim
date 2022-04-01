filetype on

let g:loaded_matchparen = 1
let g:loaded_matchit = 1
let g:loaded_vimball = 1
let g:loaded_vimballPlugin = 1
let g:loaded_rrhelper = 1
let g:loaded_netrwPlugin = 1
let g:loaded_tutor_mode_plugin = 1
let g:loaded_python3_provider = 0
let g:loaded_node_provider = 0
let g:loaded_perl_provider = 0
let g:loaded_ruby_provider = 0

lua require('impatient')
let g:markdown_fenced_languages = [ 'javascript', 'js=javascript', 'json=javascript', 'ts=typescript' ]

set list
set listchars+=lead:·,space:·,trail:·,eol:↴
set shada='100,<50,s10,:100,/100,h

set wildignore+=*.pyc,__pycache__,*~,#*#,.git,*.o,node_modules

let &statusline = ' %{statusline#obsession()}%<%f [%{&filetype ==# "" ? "none" : &filetype}]%{statusline#lsp()} %m%r%=%{statusline#git()}%10.(%l:%c%V%)%6.P '

set number
set relativenumber
set expandtab
set shiftwidth=4
set softtabstop=4
set foldlevel=99
set breakindent
set linebreak

set termguicolors
set clipboard=unnamedplus

set pumheight=10
set pumblend=17
set wildmode=longest:full,full
set inccommand=split
set signcolumn=yes

" set colorcolumn=+1
set cursorline

set ignorecase
set smartcase

set splitright
set splitbelow
set lazyredraw
set scrolloff=10
set sidescrolloff=2
set mouse=a
set updatetime=250
set timeoutlen=500

set completeopt=menu,menuone
set shortmess+=c
set confirm
set backspace=indent,eol,nostop

set grepprg=rg\ --vimgrep\ --smart-case
set grepformat^=%f:%l:%c:$m

set noswapfile
set undofile

set diffopt+=indent-heuristic

map <Space> <Nop>
let g:mapleader="\<Space>"
let g:maplocalleader="\<Space>"

nnoremap gq<CR> mzHmygggqG`yzt`z
nnoremap <silent> <Leader>sws <Cmd>%s/\s\+$<CR>

nnoremap <Leader>tj :Tjump /
nnoremap <Leader>ef :find<Space>
nnoremap <C-W><Space>ef :sp %:p:h<Space>
nnoremap <Leader>b :ls<CR>:b<Space>
nnoremap <C-W><Space>b :ls<CR>:sb<Space>

" tpope's impatient mappings
nnoremap <expr> [a '<Cmd>' . v:count1 . 'prev<CR>'
nnoremap <expr> ]a '<Cmd>' . v:count1 . 'next<CR>'
nnoremap <expr> [b '<Cmd>' . v:count1 . 'bprev<CR>'
nnoremap <expr> ]b '<Cmd>' . v:count1 . 'bnext<CR>'
nnoremap <expr> [l '<Cmd>' . v:count1 . 'lprev<CR>'
nnoremap <expr> ]l '<Cmd>' . v:count1 . 'lnext<CR>'
nnoremap <expr> [q '<Cmd>' . v:count1 . 'cprev<CR>'
nnoremap <expr> ]q '<Cmd>' . v:count1 . 'cnext<CR>'
nnoremap <expr> [t '<Cmd>' . v:count1 . 'tprev<CR>'
nnoremap <expr> ]t '<Cmd>' . v:count1 . 'tnext<CR>'
nnoremap <expr> [<C-L> '<Cmd>' . v:count1 . 'lolder<CR>'
nnoremap <expr> ]<C-L> '<Cmd>' . v:count1 . 'lnewer<CR>'
nnoremap <expr> [<C-Q> '<Cmd>' . v:count1 . 'colder<CR>'
nnoremap <expr> ]<C-Q> '<Cmd>' . v:count1 . 'cnewer<CR>'

nnoremap [A <Cmd>first<CR>
nnoremap ]A <Cmd>last<CR>
nnoremap [B <Cmd>bfirst<CR>
nnoremap ]B <Cmd>blast<CR>
nnoremap [L <Cmd>lfirst<CR>
nnoremap ]L <Cmd>llast<CR>
nnoremap [Q <Cmd>cfirst<CR>
nnoremap ]Q <Cmd>clast<CR>
nnoremap [T <Cmd>tfirst<CR>
nnoremap ]T <Cmd>tlast<CR>

nnoremap <expr> [e '<Cmd>.move --' . v:count1 . '<CR>'
nnoremap <expr> ]e '<Cmd>.move +' . v:count1 . '<CR>'
xnoremap <expr> [e ':move --' . v:count1 . '<CR>gv'
xnoremap <expr> ]e ':move +' . (v:count1 + line('''>') - line('''<')) . '<CR>gv'
nnoremap [<Space> <Cmd>put! =repeat(nr2char(10), v:count1)<CR><CR>:']+1<CR>
nnoremap ]<Space> <Cmd>put =repeat(nr2char(10), v:count1)<CR><CR>:'[-1<CR>
nnoremap <expr> yod '<Cmd>' . (&diff ? 'diffoff' : 'diffthis') . '<CR>'
nnoremap yoc <Cmd>setlocal cursorline!<Bar>set cul?<CR>
nnoremap yo<Bar> <Cmd>setlocal cursorcolumn!<Bar>set cuc?<CR>
nnoremap yos <Cmd>setlocal spell!<Bar>set spell?<CR>
nnoremap yol <Cmd>setlocal list!<Bar>set list?<CR>
nnoremap <expr> yoC '<Cmd>setlocal colorcolumn=' . (&colorcolumn == '' ? '+1' : '') . '<CR>'

nmap <Leader>qs <Plug>(qf_qf_switch)
nmap <Leader>qt <Plug>(qf_qf_toggle)
nmap <Leader>ls <Plug>(qf_loc_switch)
nmap <Leader>lt <Plug>(qf_loc_toggle)

cnoremap <expr> <C-P> wildmenumode() ? '<C-P>' : '<Up>'
cnoremap <expr> <C-N> wildmenumode() ? '<C-N>' : '<Down>'
cnoremap <expr> <C-J> pumvisible() ? '<Down><Tab>' : '<C-J>'
cnoremap <expr> <C-K> pumvisible() ? '<Up><Tab>' : '<C-K>'
tnoremap <Esc> <C-\><C-n>

function! Sort(type, ...) abort
    '[,']sort
    call setpos('.', getpos("''"))
endfunction
nnoremap gs m'<Cmd>set operatorfunc=Sort<CR>g@
xnoremap gs :sort<CR>

augroup init
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank { higroup='IncSearch', timeout=100, on_macro=true }
    autocmd InsertEnter,WinLeave,FocusLost * setlocal nocursorline
    autocmd InsertLeave,WinEnter,FocusGained * if mode() !=# 'i' | let &l:cursorline=1 | endif
    autocmd TermClose *
            \ if !v:event.status |
            \   let info = nvim_get_chan_info(&channel) |
            \   if get(info, 'argv', []) ==# [&shell] |
            \       exec 'bdelete! ' .. expand('<abuf>') |
            \   endif |
            \ endif
augroup END
