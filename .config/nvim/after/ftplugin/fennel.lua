local bo = vim.bo
local b = vim.b
b.undo_ftplugin = b.undo_ftplugin or ''
bo.commentstring = ";%s"
bo.comments = ":;"
bo.lisp = true
bo.lispwords = "accumulate,collect,do,doto,each,fn,for,icollect,lambda,let,macro,macros,match,when,while,with-open,λ"
b.undo_ftplugin = b.undo_ftplugin .. "|setl cms< com< lisp< lw<"
