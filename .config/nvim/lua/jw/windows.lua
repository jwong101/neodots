local winfo = vim.fn.getwininfo()
local c_winid = vim.fn.win_getid()
local winids = vim.api.nvim_tabpage_list_wins(0)
winids = vim.tbl_filter(function(idx)

end, winids)

local u = require('jw.utils')
local has = u.partial(vim.tbl_contains, winfo)



for _, win in ipairs(winfo) do
  if win then
    return ''
  end
  return ''
end
-- vim.pretty_print(winfo)
