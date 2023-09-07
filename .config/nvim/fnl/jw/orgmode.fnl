(import-macros {: with-mod} :jw.general)

(with-mod [orgmode :orgmode]
  (orgmode.setup {:org_agenda_files ["~/Documents/notes/*"]
                  :org_default_notes_file "~/Documents/notes/refile.org"}))
