local function nsetkey(...)
    return vim.keymap.set('n', ...)
end

local diagnostic = vim.diagnostic
nsetkey('[g', diagnostic.goto_prev)
nsetkey(']g', diagnostic.goto_next)
nsetkey('<leader>gs', diagnostic.open_float)
nsetkey('<leader>gl', diagnostic.setloclist)
nsetkey('<leader>gq', diagnostic.setqflist)
