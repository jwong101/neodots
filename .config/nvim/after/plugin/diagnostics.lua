local diagnostic = vim.diagnostic
local u = require 'jw.utils'
u.nmap('[g', diagnostic.goto_prev)
u.nmap(']g', diagnostic.goto_next)
u.nmap('<leader>gs', diagnostic.open_float)
u.nmap('<leader>gl', diagnostic.setloclist)
u.nmap('<leader>gq', diagnostic.setqflist)

local nearest_diag = require('textobj-diagnostic').nearest_diag
u.nmap('<leader>gj', nearest_diag)

--[[ local lines = require 'lsp_lines' ]]
--[[ lines.setup() ]]
--[[ vim.diagnostic.config { ]]
--[[   virtual_text = false, ]]
--[[ } ]]
--[[ vim.g.lsp_lines_enabled = true ]]
--[[ vim.keymap.set('n', 'yov', function() ]]
--[[   local swap = not vim.g.lsp_lines_enabled ]]
--[[   vim.g.lsp_lines_enabled = swap ]]
--[[   vim.diagnostic.config { virtual_lines = swap } ]]
--[[ end, { remap = true }) ]]
--[[ local toggle_lines = vim.api.nvim_create_augroup('ToggleLspLines', {}) ]]
--[[ vim.api.nvim_create_autocmd({ 'InsertEnter' }, { ]]
--[[   group = toggle_lines, ]]
--[[   desc = 'Toggle LSP lines on insert', ]]
--[[   callback = function(_) ]]
--[[     vim.diagnostic.config { virtual_lines = false } ]]
--[[   end, ]]
--[[ }) ]]
--[[ vim.api.nvim_create_autocmd({ 'InsertLeave' }, { ]]
--[[   group = toggle_lines, ]]
--[[   desc = 'Toggle LSP lines on insert', ]]
--[[   callback = function(_) ]]
--[[     if vim.g.lsp_lines_enabled then ]]
--[[       vim.diagnostic.config { virtual_lines = true } ]]
--[[     end ]]
--[[   end, ]]
--[[ }) ]]
