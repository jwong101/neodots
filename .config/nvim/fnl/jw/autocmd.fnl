(λ setl [opt ?val]
  (assert-compile (sym? opt) "opt should be a plain symbol" opt)
  (let [opt (tostring opt)]
    (if ?val
        `(tset vim.opt_local ,opt ,?val)
        (match (opt:gsub "&$" "")
          (where (o n) (> n 0) `(let [{:default default#} (vim.api.nvim_get_option_info ,o)]
                                  (tset vim.opt_local ,o default#))
                 _ (match (opt:gsub "^no" "")
                     (o n) `(tset vim.opt_local ,o ,(=n 0))))))))
(λ setl+= [opt val]
  (assert-compile (sym? opt) "opt should be a plain symbol" opt)
  `(: (. vim.opt_local ,(tostring opt)) :append ,val))

(λ setl^= [opt val]
  (assert-compile (sym? opt) "opt should be a plain symbol" opt)
  `(: (. vim.opt_local ,(tostring opt)) :prepend ,val))

(fn with-module [[binding name] ...]
  (assert-compile (= (type name) :string) "name should be a string" name)
  `(match (pcall require ,name)
    (true ,binding) (do ,...)))

{: setl
 : setl+=
 : setl^=
 : with-module}
