local ok, gitsigns = pcall(require, 'gitsigns')
if not ok then
  return
end

-- gitsigns.setup {
--   signs = {
--     add = {
--       hl = 'GitGutterAdd',
--       text = '+',
--     },
--     change = {
--       hl = 'GitGutterChange',
--       text = '~',
--     },
--     delete = {
--       hl = 'GitGutterDelete',
--       text = '_',
--     },
--     topdelete = {
--       hl = 'GitGutterDelete',
--       text = '‾',
--     },
--     changedelete = {
--       hl = 'GitGutterChange',
--       text = '~',
--     },
--   },
--   keymaps = {
--     noremap = true,
--     buffer = true,
--     ['n [c'] = { expr = true, "&diff ? '[c' : '<Cmd>Gitsigns prev_hunk<CR>'" },
--     ['n ]c'] = { expr = true, "&diff ? ']c' : '<Cmd>Gitsigns next_hunk<CR>'" },
--   },
-- }
--
gitsigns.setup {
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns
    local u = require 'jw.utils'
    local nmap = u.bmap({ 'n' }, { buffer = bufnr })
    local nvmap = u.bmap({ 'n', 'v' }, { buffer = bufnr })
    local oxmap = u.bmap({ 'o', 'x' }, { buffer = bufnr })

    nmap(']c', function()
      if vim.wo.diff then
        return ']c'
      end
      vim.schedule(function()
        gs.next_hunk()
      end)
      return '<Ignore>'
    end, { expr = true })

    nmap('[c', function()
      if vim.wo.diff then
        return '[c'
      end
      vim.schedule(function()
        gs.prev_hunk()
      end)
      return '<Ignore>'
    end, { expr = true })

    nvmap('<leader>zs', ':Gitsigns stage_hunk<CR>')
    nmap('<leader>zS', gs.stage_buffer)
    nmap('<leader>zR', gs.reset_buffer)
    nmap('<leader>zp', gs.preview_hunk)
    oxmap('ih', ':<C-U>Gitsigns select_hunk<CR>')
    nvmap('<leader>zr', ':Gitsigns reset_hunk<CR>')
    nmap('yob', gs.toggle_current_line_blame)
    nmap('yoB', function()
      gs.blame_line { full = true }
    end)
  end,
}
