local test = { a = 1 }
test.foo = { bar = 3 }
vim.pretty_print(vim.tbl_get(test, 'foo', 'bar'))
print(#test)
print(('foo'):len())
local meta = getmetatable(test)

vim.pretty_print(meta)

local meta = {
  __call = function(self, t)
    local num = rawget(self, 'num') or 0
    vim.pretty_print(type(t))
    vim.pretty_print(num)
    rawset(self, 'num', num + 1)
  end,
}

test = setmetatable(test, meta)
test(test)
test(test)

for key, value in pairs(test) do
  print('%s %s', key, value)
end
local cnt = 0
local res = table.foreach(test, function(key, _value)
  if cnt == 0 then
    cnt = cnt + 1
    return
  end
  return cnt
end)
print(res)
