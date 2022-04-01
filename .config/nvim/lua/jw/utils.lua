local M = {}

function M.nmap(lhs, rhs, opts)
  opts = opts or {}
  vim.keymap.set('n', lhs, rhs, opts)
end

function M.vmap(lhs, rhs, opts)
  opts = opts or {}
  vim.keymap.set('v', lhs, rhs, opts)
end

function M.bmap(mode, opts)
  return function (lhs, rhs, extra)
    opts = opts or {}
    vim.tbl_extend("force", opts, extra)
    vim.keymap.set(mode, lhs, rhs, opts)
  end
end

function M.partial(func, arg)
  return function(...)
    return func(arg, ...)
  end
end

function M.lambda(func, ...)
  local varg = arg
  return function(...)
    return func(unpack(varg), ...)
  end
end

function M.reload(module)
  package.loaded[module] = nil
  return require(module)
end

return M
