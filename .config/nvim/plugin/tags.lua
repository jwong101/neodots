-- Based off: https://github.com/gpanders/dotfiles/blob/master/.config/nvim/plugin/tags.fnl
local function find_tags(query)
  if string.match(query, "^/") then
    return vim.fn.taglist(string.sub(query, 2))
  else
    return vim.fn.taglist(string.format("^%s$", query))
  end
end

local function tags_to_quickfix(tags, title)
  local qf = {}
  for i, v in ipairs(tags) do
    local cmd = v.cmd
    qf[i] = {text = v.name, type = v.kind, filename = v.filename, nr = i, pattern = "\\M" .. string.sub(cmd, 2, string.len(cmd) - 1)}
  end
  vim.fn.setqflist({}, " ", {items = qf, context = qf, title = title, quickfixtextfunc = "tags#qftf"})
  vim.cmd "copen"
end

local function tselect(arg)
  local tags = find_tags(arg)
  tags_to_quickfix(tags, arg)
  return tags
end

local function tjump(arg)
  local tags = tselect(arg)
  if #tags == 1 then
    vim.cmd(string.format("tjump %s", tags[1].name))
  end
end


local addcmd = vim.api.nvim_add_user_command
addcmd('Tselect', function(cargs) tselect(cargs.args) end, { nargs = 1, complete = 'tag' })
addcmd('Tjump', function(cargs) tjump(cargs.args) end, { nargs = 1, complete = 'tag' })
