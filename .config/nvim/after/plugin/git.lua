local ok, gitsigns = pcall(require, 'gitsigns')
if not ok then
  return
end

gitsigns.setup {
  signs = {
    add = {
      hl = 'GitGutterAdd',
      text = '+',
    },
    change = {
      hl = 'GitGutterChange',
      text = '~',
    },
    delete = {
      hl = 'GitGutterDelete',
      text = '_',
    },
    topdelete = {
      hl = 'GitGutterDelete',
      text = '‾',
    },
    changedelete = {
      hl = 'GitGutterChange',
      text = '~',
    },
  },
  keymaps = {
    noremap = true,
    buffer = true,
    ["n [c"] = { expr = true, "&diff ? '[c' : '<Cmd>Gitsigns prev_hunk<CR>'"},
    ["n ]c"] = { expr = true, "&diff ? ']c' : '<Cmd>Gitsigns next_hunk<CR>'"},
  }
}
  -- on_attach = function (bufnr)
  --   local gs = package.loaded.gitsigns
  --   local u = require('jw.utils')
  --   local nmap = u.bmap('n', { buffer = bufnr })
  --   local nvmap = u.bmap({ 'n', 'v' }, { buffer = bufnr })
  --   local oxmap = u.bmap({ 'o', 'x' }, { buffer = bufnr })
  --   nmap('[c', "&diff ? '[c' : '<cmd>Gitsigns prev_hunk<CR>'", {expr=true})
  --   nmap(']c', "&diff ? ']c' : '<cmd>Gitsigns next_hunk<CR>'", {expr=true})
  --
  --   nvmap('<leader>hs', ':Gitsigns stage_hunk<CR>')
  --   nvmap('<leader>hr', ':Gitsigns reset_hunk<CR>')
  --   nmap('<leader>hS', gs.stage_buffer)
  --   nmap('<leader>hu', gs.undo_stage_hunk)
  --   nmap('<leader>hR', gs.reset_buffer)
  --   nmap('<leader>hp', gs.preview_hunk)
  --   nmap('<leader>hb', u.partial(gs.blame_line, {full=true}))
  --   nmap('<leader>hd', gs.diffthis)
  --   nmap('<leader>hD', u.partial(gs.diffthis, '~'))
  --   nmap('yod', gs.toggle_deleted)
  --   nmap('yob', gs.toggle_current_line_blame)
  --   oxmap('ih', ':<C-U>Gitsigns select_hunk<CR>')
  -- end
