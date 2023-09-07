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
  opts = opts or {}
  return function(lhs, rhs, extra)
    extra = extra or {}
    vim.tbl_extend('force', opts, extra)
    vim.keymap.set(mode, lhs, rhs, opts)
  end
end

function M.partial(func, arg)
  return function(...)
    return func(arg, ...)
  end
end

function M.leader(lhs, rhs, opts)
  opts = opts or {}
  vim.keymap.set('n', '<leader>' .. lhs, rhs, opts)
end

function M.lambda(func, ...)
  local varg = arg
  return function(...)
    return func(unpack(varg), ...)
  end
end

function M.add_cmd(...)
  return vim.api.nvim_create_user_command(...)
end

function M.buf_add_cmd(bufnr)
  return M.partial(vim.api.nvim_buf_create_user_command, bufnr)
end

function M.reload(module)
  package.loaded[module] = nil
  return require(module)
end

return M
