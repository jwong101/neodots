(local M {})

(fn M.nmap [lhs rhs ?opts]
  `(vim.keymap.set "n" ,lhs ,rhs ,?opts))

(fn M.vmap [lhs rhs ?opts]
  `(vim.keymap.set "v" ,lhs ,rhs ,?opts))

(fn M.imap [lhs rhs ?opts]
  `(vim.keymap.set "i" ,lhs ,rhs ,?opts))

(fn M.leader [lhs rhs ?opts]
  (M.nmap (.. "<leader>"  lhs) rhs ?opts))

(fn M.vleader [lhs rhs ?opts]
  (M.vmap (.."<leader>" lhs) rhs ?opts))

(fn M.cmd! [name func ?opts]
  (let [opts (or ?opts {})
        bufnr opts.?bufnr]
    (if bufnr
        `(vim.api.buf_create_user_command ,bufnr ,name ,func ,opts)
        `(vim.api.create_user_command ,name ,func ,opts))))

(fn M.with-mod [[binding name] ...]
  "Bind BINDING to module NAME and evaluate the other forms if successful."
  `(let [(loaded?# ,binding) (pcall require ,name)]
     (when loaded?# ,...)))

M
