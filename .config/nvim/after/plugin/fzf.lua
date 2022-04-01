if vim.g.loaded_fzf_lua ~= 1 then
  return
end

local loaded, fzf = pcall(require, 'fzf-lua')

if not loaded then
  return
end

local u = require 'jw.utils'



u.nmap('<leader>en', u.partial(fzf.files, {cwd = '~/.config/nvim'}))

u.nmap('<leader>ez', u.partial(fzf.files, {cwd = '~/.config/zsh'}))

u.nmap('<leader>fd', fzf.files)

u.nmap('<leader>fg', fzf.git_files)

u.nmap('<leader>sm', fzf.marks)

u.nmap('<leader>sr', fzf.registers)

u.nmap('<leader>sj', fzf.jumps)

u.nmap('<leader>th', fzf.help_tags)

u.nmap('<leader>tt', fzf.tags)

u.nmap('<leader>tb', fzf.btags)

u.nmap('<leader>sb', fzf.buffers)

u.nmap('<leader>sa', fzf.args)

u.nmap('<leader>fl', fzf.blines)

u.nmap('<leader>qf', fzf.quickfix)
u.nmap('<leader>lf', fzf.loclist)
-- u.nmap('<leader>cf', fzf.lsp_code_actions)

u.nmap('<leader>p', fzf.live_grep_native)
u.nmap('<leader>P', fzf.live_grep_resume)
u.nmap('gx', fzf.grep_cword)
u.nmap('gX', fzf.grep_cWORD)
-- vim.keymap.set({'n', 'v'}, 'gx', fzf.grep_cword)

u.nmap('<leader>j', fzf.command_history)
u.nmap('<leader>k', fzf.commands)
u.nmap('<leader>o', fzf.search_history)

-- fzf.register_ui_select()
