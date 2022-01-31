
local add_cmd = vim.api.nvim_add_user_command

add_cmd('PackerClean', function()
  package.loaded['jw.plugins'] = false
  require 'jw.plugins'.clean()
end, {})

add_cmd('PackerSync', function()
  package.loaded['jw.plugins'] = false
  require 'jw.plugins'.sync()
end, {})

