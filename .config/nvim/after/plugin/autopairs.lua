local ok, autopairs = pcall(require, 'nvim-autopairs')

if not ok then
  return
end

autopairs.setup {
  check_ts = true,
  disable_filetype = {
    "TelescopePrompt",
  },
}

local Rule = require('nvim-autopairs.rule')
local ts_conds = require('nvim-autopairs.ts-conds')

autopairs.add_rules({
  Rule("'", "'", "ocaml"):with_pair(ts_conds.is_ts_node {'string'})
})
