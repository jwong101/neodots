local ok, nv_notify = pcall(require, 'notify')

if not ok then
  return
end
nv_notify.setup {
  background_colour = '#000000',
}

local u = require 'jw.utils'

u.nmap(
  '<leader>nd',
  u.partial(nv_notify.dismiss, {
    pending = false,
  }),
  { silent = true }
)

vim.notify = function(msg, ...)
  if
    msg:match 'character_offset must be called'
    or msg:match 'warning: multiple different client offset_encodings detected for buffer, this is not supported yet'
  then
    return
  end
  nv_notify(msg, ...)
end
