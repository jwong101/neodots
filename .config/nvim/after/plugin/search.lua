local keymap = vim.keymap.set

local function mapsearch(op)
  keymap('', op, string.format([[<plug>(asterisk-%s)<cmd>lua require('hlslens').start()<cr>]], o), {remap = true})
end

local function start_hlslens(dir)
  keymap('n', dir, function()
    vim.fn.execute('normal! ' .. vim.v.count1 .. dir)
    require('hlslens').start()
  end, {silent = true})
end

local function append_hlslens(cmd)
  keymap('n', cmd, cmd .. [[<cmd>lua require('hlslens').start()<cr>]])
end

start_hlslens('n')
start_hlslens('N')

append_hlslens '*'
append_hlslens '#'
append_hlslens 'g*'
append_hlslens 'g#'

mapsearch 'z*'
mapsearch 'z#'
mapsearch 'gz*'
mapsearch 'gz#'
