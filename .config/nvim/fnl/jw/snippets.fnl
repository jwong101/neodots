(import-macros "jw.autocmd")

(with-module [ls :luasnip]
  (ls.config.setup {:history true
                    :updateevents "TextChanged,TextChangedI"
                    :enable_autosnippets true}))
    
