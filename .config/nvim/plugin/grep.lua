-- if vim.g.loaded_customgrep == 1 then
--   return
-- end

-- vim.g.loaded_customgrep = 1

local Job = require 'plenary.job'

-- lua is in desperate need of macros...
local function setlist(loclist)
  local list = 'grep'
  if loclist then
    list = 'lgrep'
  end
  return function(...)
    if loclist then
      vim.fn.setloclist(0, ...)
      vim.cmd 'lopen'
    else
      vim.fn.setqflist(...)
      vim.cmd 'copen'
    end
    vim.cmd('doautocmd QuickFixCmdPost ' .. list)
  end
end

local function grep(loclist)
  return function(fargs)
    local f_args = vim.fn.expandcmd(fargs.args)
    f_args = vim.split(f_args, ' ', { trimempty = true })
    if #f_args == 1 then
      f_args[#f_args + 1] = vim.fn.expand '%'
    end

    local grep_args = vim.list_extend({ '--vimgrep', '--smart-case' }, f_args)
    local list = 'grep'
    if loclist then
      list = 'lgrep'
    end
    vim.cmd('doautocmd QuickFixCmdPre ' .. list)
    local cwd = vim.fn.getcwd()
    Job:new({
      command = 'rg',
      args = grep_args,
      cwd = cwd,
      on_exit = function(self, return_val)
        if return_val ~= 0 then
          return
        end
        local result = self:result()
        local setl = setlist(loclist)
        vim.defer_fn(function()
          setl({}, ' ', {
            efm = vim.o.grepformat,
            title = 'Ripgrep',
            nr = '$',
            lines = result,
          })
        end, 0)
      end,
    }):start()
  end
end

local addcmd = vim.api.nvim_create_user_command
addcmd('Grep', grep(false), { nargs = '+', complete = 'file_in_path' })
addcmd('LGrep', grep(true), { nargs = '+', complete = 'file_in_path' })
vim.keymap.set('n', 'g/', ':Grep ', { silent = false })
vim.keymap.set('x', 'g/', 'y:<C-U>Grep <C-R>"', { silent = false })
