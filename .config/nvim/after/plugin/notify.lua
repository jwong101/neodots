local ok, notify = pcall(require, 'notify')

if not ok then
  return
end

local u = require 'jw.utils'

u.nmap('<leader>nd', u.partial(notify.dismiss, {
  pending = false,
}) , { silent = true })

vim.notify = notify
