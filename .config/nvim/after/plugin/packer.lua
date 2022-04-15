local u = require('jw.utils')
local add_cmd = u.add_cmd

add_cmd('PackerClean', function()
  package.loaded['jw.plugins'] = false
  require 'jw.plugins'.clean()
end, {})

add_cmd('PackerSync', function()
  package.loaded['jw.plugins'] = false
  require 'jw.plugins'.sync()
end, {})

