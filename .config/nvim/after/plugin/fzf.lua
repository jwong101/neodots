if vim.g.loaded_fzf_lua ~= 1 then
  return
end

local loaded, fzf = pcall(require, 'fzf-lua')

if not loaded then
  return
end

local nsetk = function(...)
  vim.keymap.set('n', ...)
end


nsetk('<leader>en', function()
  fzf.files({cwd = '~/.config/nvim'})
end)

nsetk('<leader>ez', function()
  fzf.files({cwd = '~/.config/zsh'})
end)

nsetk('<leader>fd', function()
  fzf.files()
end)

nsetk('<leader>fg', function()
  fzf.git_files()
end)

nsetk('<leader>sm', function()
  fzf.marks()
end)

nsetk('<leader>sr', function()
  fzf.registers()
end)

nsetk('<leader>sj', function()
  fzf.jumps()
end)

nsetk('<leader>th', function()
  fzf.help_tags()
end)

nsetk('<leader>tt', function()
  fzf.tags()
end)

nsetk('<leader>tb', function()
  fzf.btags()
end)

nsetk('<leader>sb', function()
  fzf.buffers()
end)

nsetk('<leader>sa', function()
  fzf.args()
end)

nsetk('<leader>fl', function()
  fzf.blines()
end)

nsetk('<leader>qf', fzf.quickfix)
nsetk('<leader>lf', fzf.loclist)
-- nsetk('<leader>cf', fzf.lsp_code_actions)

nsetk('<leader>p', fzf.live_grep_native)
nsetk('<leader>P', fzf.live_grep_resume)
nsetk('gx', fzf.grep_cword)
nsetk('gX', fzf.grep_cWORD)
-- vim.keymap.set({'n', 'v'}, 'gx', fzf.grep_cword)

nsetk('<leader>j', fzf.command_history)
nsetk('<leader>k', fzf.commands)
nsetk('<leader>o', fzf.search_history)

-- fzf.register_ui_select()
