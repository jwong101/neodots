;;; init.el --- my initialization file. -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)
(setq user-full-name "Joshua Wong")
(setq user-mail-address "joshuawong@anticentri.st")

(setq straight-use-package-by-default t
      straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-host-usernames '((github . "jwong101")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(require 'cl-lib)
(pixel-scroll-precision-mode 1)
(setq inhibit-startup-message t)
(setq enable-recursive-minibuffers t)
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(setq x-gtk-use-system-tooltips nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq frame-inhibit-implied-resize t)
(setq idle-update-delay 1.0)
(setq command-line-ns-option-alist nil)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)
(setq create-lockfiles nil
      make-backup-files nil)

(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
(defun my/setup-prog-hooks ()
  "Functions to run before entering prog mode."
  (display-line-numbers-mode)
  (column-number-mode)
  (electric-pair-local-mode)
  (setq show-trailing-whitespace t))

(dolist (m '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook m #'my/setup-prog-hooks))

(setq-default indent-tabs-mode nil
	      tab-width 4)
(setq-default tab-always-indent nil)
(with-eval-after-load 'tabify
  (setq tabify-regexp "^\t* [ \t]+"))
(setq-default word-wrap t)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq kill-do-not-save-duplicates t)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(defconst my-emacs-cache-dir (concat (getenv "XDG_CACHE_HOME") "/emacs/")
  "Place to put temporary files.")

(declare-function straight-use-package "straight" t)

(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package)
  (require 'cl-lib))

(cl-pushnew (expand-file-name "lisp" user-emacs-directory)
            load-path :test #'string=)

(use-package diminish)

(use-package general
  :demand t)

(eval-and-compile
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-local #'general-setq-local)
  (defalias 'gsetq-default #'general-setq-default))

(use-package magit
  :bind (("C-x g" . magit-status))
   ("C-x C-g" . magit-status))

(use-package forge
  :after magit)

(use-package explain-pause-mode
  :disabled
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  :config
  (explain-pause-mode))

(use-package goto-chg)

(use-package evil
  :after goto-chg
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (general-setq evil-want-Y-yank-to-eol t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (general-setq evil-move-cursor-back nil)
  (general-setq evil-move-beyond-eol nil)
  (setq evil-search-module #'evil-search)
  (setq evil-ex-search-persistent-highlight nil)
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-C-u-scroll t)
  (evil-kbd-macro-suppress-motion-error t)
  :config
  (evil-mode 1)
  (setq evil-normal-state-cursor '(box "orchid")
        evil-normal-state-cursor '(box "Orange")
        evil-motion-state-cursor '(box "YellowGreen")
        evil-insert-state-cursor '(bar "Red")
        evil-emacs-state-cursor '(bar "Blue")
        evil-visual-state-cursor '(box "#F86155")))

(use-package evil-escape
  :straight (evil-escape :type git :host github :repo "hlissner/evil-escape")
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(vterm-mode treemacs-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  (defun my--evil-inhibit-escape-in-minibuffer-h ()
    "inhibit evil escape in the minibuffer"
    (minibufferp))
  (add-hook 'evil-escape-inhibit-functions #'my--evil-inhibit-escape-in-minibuffer-h))

(general-auto-unbind-keys)

(general-evil-setup)

(general-create-definer my/leader
  :states 'normal
  :keymaps 'override
  :prefix "SPC")

(my/leader
  "u" #'universal-argument
  "j" #'consult-buffer
  "ll" #'consult-imenu
  "LL" #'consult-imenu-multi
  "rg" #'consult-ripgrep)

(my/leader
  "wf" #'find-file-other-window
  "wd" #'dired-other-window
  "wp" #'project-other-window-command
  "wj" #'consult-buffer-other-window
  "wc" #'clone-indirect-buffer-other-window
  "w]" #'xref-find-definitions-other-window
  "wm" #'compose-mail-other-window
  "tn" #'tab-new)

(keymap-set 'evil-window-map "C-d" #'scroll-other-window)
(keymap-set 'evil-window-map "C-u" #'scroll-other-window-down)



(general-define-key
 :states '(normal visual)
 :keymaps '(override lsp-mode-map)
 "gd" #'lsp-find-definition
 "K" #'lsp-ui-doc-glance)

(general-define-key
 :states '(normal insert)
 :keymaps '(override lsp-mode-map)
 "M-k" #'lsp-signature-toggle-full-docs)


(general-define-key
 :states 'insert
 :keymaps 'override
 "C-a" #'evil-beginning-of-line
 "C-e" #'evil-end-of-line)

(my/leader
  "pp" #'project-switch-project)

(general-setq uniquify-buffer-name-style 'forward)
(general-setq find-file-visit-truename t
              vc-follow-symlinks t)
(general-setq bidi-inhibit-bpa t)
(general-setq-default bidi-display-reordering 'left-to-right
                      bidi-paragraph-direction 'left-to-right)

(general-setq x-underline-at-descent-line t)
(general-setq-default cursor-in-non-selected-windows nil)
(general-setq highlight-nonselected-windows nil)
(general-setq auto-save-default nil)
(general-setq kill-do-not-save-duplicates t)
(general-setq adaptive-fill-mode t)
(blink-cursor-mode -1)
(general-add-advice 'startup-echo-area-message :override #'ignore)

(minibuffer-depth-indicate-mode)

(general-create-definer my-g-pre
  :states 'normal
  :keymaps 'override
  :prefix "g")

(my-g-pre
 "m" #'bookmark-set
 "'" #'bookmark-jump)

(general-define-key
 :states '(insert emacs)
 :keymaps '(override corfu-map)
 "C-h" #'corfu-show-documentation)

(defun my-visible-buffers ()
  "Return a list of visible buffers."
(mapcar #'window-buffer
        (seq-mapcat (lambda (frame) (window-list frame 1))
                    (visible-frame-list))))

(use-package autorevert
  :straight (:type built-in)
  :hook (after-save . my--auto-revert-buffers-h)
  :hook (my/switch-buffer . my--auto-revert-buffer-h)
  :hook (my/switch-window . my--auto-revert-buffer-h)
  :config
  (setq auto-revert-verbose t
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        revert-without-query (list "."))

  (defun my--auto-revert-buffer-h ()
    "Auto revert the current buffer if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun my--auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (my-visible-buffers))
      (with-current-buffer buf
        (my--auto-revert-buffer-h)))))

;; shamelessly stolen from the doom configs
(defvar my/switch-buffer-hook nil
  "A list of hooks to run after changing the current buffer.")

(defvar my/inhibit-switch-buffer-hooks nil
  "Letvar for inhibiting `my/switch-buffer-hook'. Do not set this directly.")

(defun my/run-switch-buffer-hooks-a (orig-fn buffer-or-name &rest args)
  "Run hooks from `my/switch-buffer-hook' in buffer if it has changed."
  (let ((gc-cons-threshold most-positive-fixnum))
    (if (or my/inhibit-switch-buffer-hooks
            (eq (current-buffer) (get-buffer buffer-or-name))
            (and (eq orig-fn #'switch-to-buffer) (car args)))
        (apply orig-fn buffer-or-name args)
      (let ((my/inhibit-switch-buffer-hooks t))
        (when-let (buffer (apply orig-fn buffer-or-name args))
          (with-current-buffer (if (windowp buffer)
                                   (window-buffer buffer)
                                 buffer)
            (run-hooks 'my/switch-buffer-hook))
          buffer)))))

(defun my/run-switch-to-next-prev-buffers-a (orig-fn &rest args)
  "Run hooks from `my/switch-buffer-hook' in the new buffer."
  (let ((gc-cons-threshold most-positive-fixnum))
    (if my/inhibit-switch-buffer-hooks
        (apply orig-fn args)
      (let ((my/inhibit-switch-buffer-hooks t))
        (when-let (buffer (apply orig-fn args))
          (with-current-buffer buffer
            (run-hooks 'my/switch-buffer-hook))
          buffer)))))

(general-add-advice '(switch-to-buffer display-buffer)
                    :around #'my/run-switch-buffer-hooks-a)

(general-add-advice '(switch-to-next-buffer switch-to-prev-buffer)
                    :around #'my/run-switch-to-next-prev-buffers-a)

(defmacro my/after-buffer (&rest body)
  "Run BODY once after switching buffers or when finding a file."
  (declare (indent defun))
  `(let ((fun (lambda (&rest _)
                ,@body)))
     (general-add-hook '(my/switch-buffer-hook)
                       fun nil nil t)
     (general-add-advice 'after-find-file :before fun nil t)))

(defmacro my/after-window (&rest body)
  "Run BODY once after switching windows or when finding a file."
  (declare (indent defun))
  `(let ((fun (lambda (&rest _)
                ,@body)))
     (general-add-hook '(my/switch-buffer-hook)
                       fun nil nil t)
     (general-add-advice 'after-find-file :before fun nil t)))

(defvar my/switch-window-hook nil
  "A list of hooks to run after changing the focused windows.")

(defvar my/--inhibit-switch-window-hooks nil
  "Letvar for inhibiting `my/switch-window-hook'. Do not set this directly.")

(defvar my/--last-window nil)

(defun my/run-switch-window-hooks-h ()
  (let ((gc-cons-threshold most-positive-fixnum))
    (unless (or my/--inhibit-switch-window-hooks
                (eq my/--last-window (selected-window))
                (minibufferp))
      (let ((my/--inhibit-switch-window-hooks t))
        (run-hooks 'my/switch-window-hook)
        (setq my/--last-window (selected-window))))))

(general-add-hook 'buffer-list-update-hook #'my/run-switch-window-hooks-h)

(defvar my/switch-frame-hook nil
  "A list of hooks to run after changing the focused frame.")

(defvar my/--inhibit-switch-frame-hooks nil
  "Letvar for inhibiting `my/switch-frame-hook'. Do not set this directly.")

(defvar my/--last-frame nil)

(defun my/run-switch-frame-hooks-h (&rest _)
  (unless (or my/--inhibit-switch-frame-hooks
              (eq my/--last-frame (selected-frame))
              (frame-parameter nil 'parent-frame))
    (let ((my/--inhibit-switch-frame-hooks t))
      (run-hooks 'my/switch-frame-hook)
      (setq my/--last-frame (selected-frame)))))

;; (general-add-hook 'focus-in-hook #'my/run-switch-frame-hooks-h)
(general-add-advice 'after-focus-change-function :before #'my/run-switch-frame-hooks-h)

(use-package direnv
  :config
  (direnv-mode))

(use-package evil-collection
  :init
  (setq evil-want-keybinding nil)
  (setq evil-collection-outline-enable-in-minor-mode-p nil)
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys nil nil)
  (general-define-key
   :states '(visual operator)
   :keymaps 'evil-outer-text-objects-map
   "c" #'evilnc-outer-commenter)

  (general-define-key
   :states '(visual operator)
   :keymaps 'evil-inner-text-objects-map
   "c" #'evilnc-inner-commenter)

  (general-nmap "gc" (general-key-dispatch #'evilnc-comment-operator
                       "gc" #'evilnc-comment-or-uncomment-lines))

  (general-vmap "gc" #'comment-or-uncomment-region))

(use-package evil-visualstar
  :after evil
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

(use-package evil-args
  :after evil
  :defer 2
  :bind (:map evil-inner-text-objects-map
    ("a" . evil-inner-arg)
    :map evil-outer-text-objects-map
    ("a" . evil-outer-arg)
    :map evil-normal-state-map
    ("H" . evil-backward-arg)
    ("L" . evil-forward-arg)
    :map evil-motion-state-map
    ("H" . evil-backward-arg)
    ("L" . evil-forward-arg)))

(use-package evil-indent-plus
  :after evil
  :defer 2
  :bind (:map evil-inner-text-objects-map
   ("i" . evil-indent-plus-i-indent)
   ("I" . evil-indent-plus-i-indent-up)
   ("J" . evil-indent-plus-i-indent-up-down)
   :map evil-outer-text-objects-map
   ("i" . evil-indent-plus-a-indent)
   ("I" . evil-indent-plus-a-indent-up)
   ("J" . evil-indent-plus-a-indent-up-down)))

(use-package evil-goggles
  :after evil
  :init
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  :config
  (evil-goggles-mode))

(use-package recentf
  :init
  (general-add-advice '(after-find-file consult-buffer)
                      :before
                      (lambda (&rest _)
                        (recentf-mode)
                        nil
                        t))
  :config
  (general-setq recentf-max-saved-items 1000)
  (defun my--recent-file-truename (file)
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
        (abbreviate-file-name (file-truename (tramp-file-name-localname file)))
      file))
  (add-to-list 'recentf-filename-handlers #'my--recent-file-truename)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)
  (setq recentf-auto-cleanup nil)
  (general-add-hook 'kill-emacs-hook #'recentf-cleanup)
  (general-add-hook '(my/switch-window-hook write-file-functions)
                    (progn (defun my--recentf-touch-buffer-h ()
                             "Bump file in recent file list if it was switched or written to"
                             (when buffer-file-name
                               (recentf-add-file buffer-file-name))
                               nil)
                           #'my--recentf-touch-buffer-h))
  (require 'jw-lib)
  (jw-add-hook! dired-mode-hook
    (defun my--recentf-add-dired-directory-h ()
      "Add dired directories to recentf file list."
      (recentf-add-file default-directory))))

(setq find-file-suppress-same-file-warnings t)
(gsetq vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                    vc-ignore-dir-regexp
                                    tramp-file-name-regexp
                                    "[/\\\\]node_modules"))

(general-with-package 'compile
  (gsetq compilation-always-kill t
         compilation-ask-about-save nil
         compilation-scroll-output 'first-error)
  (defun my--colorize-compilation-buffer ()
    "Colorize the compilation buffer."
    (require 'ansi-color)
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (general-add-hook 'compilation-filter-hook #'my--colorize-compilation-buffer))

(general-with 'debug
  (general-def 'normal debugger-mode-map
    "q" #'debugger-quit)
  (general-after 'evil
    (general-add-hook 'debugger-mode-hook #'evil-normalize-keymaps)))

(use-package corfu
  :after vertico
  :demand
  :init
  (setq corfu-cycle t)
  (corfu-global-mode)
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound
and Vertico is not bound"
    (when (and (where-is-internal #'completion-at-point (list (current-local-map)))
               (not (bound-and-true-p vertico--input)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer)
  (general-define-key
   :states '(insert emacs)
   :keymaps '(override completion-in-region-mode-map corfu-map)
   "C-j" #'next-line
   "C-k" #'previous-line)
  (defun +my--corfu-insert-send ()
    "Insert corfu candidate and send input if in eshell-mode
or comint mode."
    (interactive)
    (corfu-insert)
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode) (fboundp 'comint-send-input))
      (comint-send-input))))
  (keymap-set corfu-map "RET" #'+my--corfu-insert-send)
  :bind
  (:map corfu-map
	    ("SPC" . corfu-insert-separator)))

(use-package corfu-doc
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
              ("M-p" . corfu-doc-scroll-down)
              ("M-n" . corfu-doc-scroll-up)
              ("M-d" . corfu-doc-toggle)))

(use-package pcmpl-args)

(use-package svg-lib)

(use-package kind-icon
  :after (corfu svg-lib)
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package lsp-ui
  :after lsp-mode)

(use-package tablist)

(use-package pdf-tools
  :after tablist
  :mode ("\\.\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (defun my--pdf-supress-large-file-prompts-a (size op-type filename &optional offer-raw)
    "Silence \"File *.pdf is large... prompts for pdfs."
    (string-match-p "\\.pdf\\'" filename))

  (advice-add #'abort-if-file-too-large
              :before-until
              #'my--pdf-supress-large-file-prompts-a)
  :config
  (pdf-tools-install-noverify)
  (general-define-key
   :states 'normal
   :keymaps '(pdf-view-mode-map)
   "q" #'kill-current-buffer)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))

(use-package saveplace
  :config
  (defun my--dont-prettify-saveplace-cache-a (fn)
    "Don't prettify the cache."
    (cl-letf ((#'pp #'prin1))
      (funcall fn)))
  (advice-add #'save-place-alist-to-file :around
              #'my--dont-prettify-saveplace-cache-a))

(use-package saveplace-pdf-view
  :after pdf-view)

(use-package nov
  :after mixed-pitch
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (general-add-hook 'nov-mode-hook '(mixed-pitch-mode visual-line-mode visual-fill-column-mode))
  :config
  (setq nov-text-width t)
  (setq nov-variable-pitch nil))

(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures t '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                      "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                      "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                      "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                      "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                      "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                      ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                      "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                      "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                      "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                      "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  (global-ligature-mode t))

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-indent-method-chain t)
  (setq rustic-analyzer-command '("~/.local/bin/rust-analyzer")))

(use-package orderless
  :init
  (setq completion-styles '(orderless))
  completion-category-defaults nil
  completion-category-overrides '((file (styles . (partial-completion)))))

(use-package cape
  :after evil
  :bind (:map evil-insert-state-map
              ("C-x C-o" . completion-at-point)
              ("C-x C-t" . complete-tag)
              ("C-x C-d" . cape-dabbrev)
              ("C-x C-f" . cape-file)
              ("C-x C-k" . cape-keyword)
              ("C-x C-s" . cape-symbol)
              ("C-x C-a" . cape-abbrev)
              ("C-x C-i" . cape-ispell)
              ("C-x C-l" . cape-line))
  :init
  (setq cape-file-directory-must-exist nil)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package tempel
  :after evil
  :bind (:map evil-insert-state-map
              ("C-x C-SPC" . tempel-complete)
              ("C-x C-m" . tempel-insert))
  :init
  (defun my--tempel-setup-capf ()
    (unless (seq-contains-p completion-at-point-functions #'tempel-expand #'eq)
      (setq-local completion-at-point-functions
                  (cons #'tempel-expand
                        completion-at-point-functions))))
  ;; (general-define-key
  ;;  :states 'insert
  ;;  :keymaps '(override tempel-map)
  ;;  "M-n" #'tempel-next
  ;;  "M-p" #'tempel-previous)

  (general-add-hook '(prog-mode text-mode conf-mode)
                    #'my--tempel-setup-capf))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :init
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask)
  (setq image-dired-dir (concat my-emacs-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        image-dired-thumb-size 150)
  :config
  (defun my--dired-no-revert-in-virtual-buffers-a (&rest args)
    "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')"
    (eq revert-buffer-function #'dired-virtual-revert))

  (advice-add #'dired-buffer-stale-p
              :before-until
              #'my--dired-no-revert-in-virtual-buffers-a)
  (keymap-set dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode))

(use-package dired-rsync
  :general (dired-mode-map "C-c C-r" #'dired-rsync))

(use-package fd-dired
  :defer t
  :commands fd-dired
  :init
  (my/leader
    "ff" #'fd-dired))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil)
  (defvar my-wdired-icons-enabled -1)

  (defun my--disable-icons-in-wdired-mode-a (&rest _)
    "Disable icons in wdired mode, since icons break file renaming."
	(setq-local my-wdired-icons-enabled
                (if all-the-icons-dired-mode
                    (progn (all-the-icons-dired-mode -1) 1) -1)))

  (defun my--restore-icons-after-wdired-mode-a (&rest _)
    "Restore icons after exiting wdired mode."
    (all-the-icons-dired-mode my-wdired-icons-enabled))

  (advice-add #'wdired-change-to-wdired-mode
              :before
              #'my--disable-icons-in-wdired-mode-a)
  (advice-add #'wdired-change-to-wdired-mode
              :after
              #'my--restore-icons-after-wdired-mode-a))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; (defun my-tabs-next (&optional arg)
;;   "switch to the next tab using centaur"
;;   (interactive "p")
;;   (if (integerp arg)
;;       (centaur-tabs-select-visible-nth-tab arg)
;;     (centaur-tabs-forward)))

;; (defun my-tabs-prev (&optional arg)
;;   "switch to the previous tab using centaur"
;;   (interactive "p")
;;   (if (integerp arg)
;;       (centaur-tabs-select-visible-nth-tab arg)
;;     (centaur-tabs-prev)))

;; (use-package centaur-tabs
;;   :bind
;;   (:map evil-normal-state-map
;; 	("g t" . my-tabs-next)
;; 	("g T" . my-tabs-prev)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-use-plists t)
  (setq lsp-idle-delay 0.1)
  (setq lsp-enable-snippet nil)
  :commands
  (lsp lsp-deferred)
  :config
  (my/leader
  :states '(normal visual)
  :keymaps '(override lsp-mode-map)
  "ls" #'consult-lsp-file-symbols
  "lS" #'consult-lsp-symbols
  "ld" #'lsp-ui-peek-find-definitions
  "la" #'lsp-code-actions-at-point
  "l;" #'lsp-find-references
  "lr" #'lsp-rename)
  :hook
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (haskell-mode . lsp-deferred)
   (haskell-literate-mode . lsp-deferred)))

(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :mode "\\.ejs\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.svelte\\'"
  :config
  (setq web-mode-enable-html-entities-fontification t)
  (setq web-mode-auto-close-style 1)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-pairing t))

(use-package json-mode
  :after lsp-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :init
  (add-hook 'json-mode-local-vars-hook #'lsp)
  (my/leader
    :states '(normal visual)
    :keymaps 'json-mode-map
    "mp" #'json-mode-show-path
    "mt" #'json-toggle-boolean
    "md" #'json-mode-kill-path
    "mx" #'json-nullify-sexp
    "ma" #'json-increment-number-at-point
    "me" #'json-decrement-number-at-point
    "mf" #'json-mode-beautify))


(use-package json-snatcher)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-gfm-additional-languages '("sh")
        markdown-nested-imenu-heading-index nil))

(use-package typescript-mode
  :commands typescript-tsx-mode
  :init
  (add-to-list 'auto-mode-alist
               (cons "\\.tsx\\'" #'typescript-tsx-mode))
  :config
  (define-derived-mode typescript-tsx-mode web-mode "Typescript-TSX"))

(use-package lsp-pyright
  :after lsp-mode)

(use-package poetry
  :after python
  :disabled t
  :hook (python-mode . poetry-tracking-mode)
  :init
  (setq poetry-tracking-strategy 'switch-buffer))

(use-package pip-requirements)

(use-package vterm
  :commands vterm
  :hook (vterm-mode . my--vterm-hook)
  :init
  (defun my--vterm-hook ()
    (setq-local confirm-kill-processes nil)
    (setq-local hscroll-margin nil))
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cc-mode
  :straight (:type built-in)
  :config
  (general-setq-default c-basic-offset 4)
  (setf (alist-get 'other c-default-style) "linux")
  (general-def 'insert c-mode-map
    "RET" #'c-indent-new-comment-line))

(use-package tuareg
  :config
  (setq tuareg-match-patterns-aligned t)
  (defun my/set-ocaml-error-regexp ()
    (setq-local
     compilation-error-regexp-alist
     (list '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
             2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face)))))
  (defun my/tuareg-hook ()
    (funcall #'my/set-ocaml-error-regexp)
    (setq tuareg-prettify-symbols-full t)
    (prettify-symbols-mode)
    (lsp-deferred))
  (add-hook 'tuareg-mode-hook #'my/tuareg-hook))

(use-package utop
  :init
  (setq utop-command "opam exec -- dune utop . -- -emacs")
  :hook (tuareg-mode . utop-minor-mode))

(use-package dune
  :straight (dune
             :type git
             :host github
             :repo "ocaml/dune"
             :files ("editor-integration/emacs/*.el")))

(use-package haskell-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package helpful
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :general
  (general-def help-map
    "RET" #'helpful-at-point
    "o" #'helpful-symbol
    "k" #'helpful-key
    "F" #'helpful-function
    "v" #'helpful-variable
    "C" #'helpful-command
    "f" #'helpful-callable)
  :init
  (setq apropos-do-all t)
  (defun my--use-helpful-a (fn &rest args)
    "Force FN to use helpful instead of the default."
    (cl-letf ((#'describe-function #'helpful-function)
              (#'describe-variable #'helpful-variable))
      (apply fn args)))
  (with-eval-after-load 'apropos
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol))))))
  :config
  (general-def 'normal helpful-mode
    :definer 'minor-mode
    "q" #'quit-window))

(general-def 'normal special-mode-map
  "q" #'quit-window)

(with-eval-after-load 'tramp
  (setq remote-file-name-inhibit-cache 60))

(use-package doom-modeline
  :after all-the-icons
  :init (doom-modeline-mode 1))

(use-package info-colors
  :after info
  :hook (Info-selection . info-colors-fontify-node))

(use-package ibuffer
  :bind ("C-x b" . ibuffer))

(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package consult
  :demand t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-local-map
              ("C-r" . consult-history))
  :init
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (my/leader
    "cl" #'consult-line
    "hm" #'consult-man
    "bb" #'consult-bookmark))

(use-package consult-dir
  :after (consult vertico)
  :commands (consult-dir consult-dir-jump-file)
  :bind (:map vertico-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file))
  :init
  (my/leader
    "cd" #'consult-dir))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode)
  (general-pushnew '(project-find-file . project-file)
                   marginalia-command-categories)
  (general-pushnew '(project-find-dir . file)
                   marginalia-command-categories)
  (general-pushnew '(project-switch-project . file)
                   marginalia-command-categories)
  (general-pushnew '("Find file" . file)
                   marginalia-prompt-categories))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package rainbow-mode)

(use-package consult-lsp
  :after marginalia)

(use-package git-modes)

;; (use-package gitignore-mode
  ;; :mode "/.dockerignore\\'")

(use-package magit-todos
  :after magit
  :config (magit-todos-mode))

(general-with-package 'compile
  (general-setq compilation-always-kill t
                compilation-ask-about-save nil))

(use-package pkgbuild-mode)

(use-package info)
  ;; :config
  ;; (general-pushnew (expand-file-name "info" user-emacs-directory)
                   ;; Info-additional-directory-list))

(use-package elisp-demos
  :defer t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package yaml-mode
  :init
  (add-hook 'yaml-mode-local-vars-hook #'lsp-deferred 'append)
  :config
  (defun my--yaml-mode-set-indent-h ()
    "Set tab width to 2"
    (setq-local tab-width yaml-indent-offset))

  (add-hook 'yaml-mode-hook #'my--yaml-mode-set-indent-h))

(use-package ws-butler
  :straight (ws-butler :type git :host github :repo "hlissner/ws-butler")
  :config
  (setq ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode))

(use-package lispy
  :hook ((lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (ielm-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (dune-mode . lispy-mode)
         (fennel-mode . lispy-mode))
  :init
  (setq lispy-close-quotes-at-end-p t)
  :config
  (general-define-key
   :keymaps 'lispy-mode-map
   :states 'normal
   "]" nil
   "[" nil))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :init
  (setq lispyville-key-theme
        '((operators normal)
          c-w
          (prettify insert)
          (atom-movement t)
          slurp/barf-lispy
          additional
          additional-insert))
  :config
  (lispyville-set-key-theme)
  (defun my--lispy-inhibit-evil-escape-fn ()
    (and lispy-mode (evil-insert-state-p)))
  (add-hook 'evil-escape-inhibit-functions #'my--lispy-inhibit-evil-escape-fn))

(use-package gcmh
  :ghook ('pre-command-hook nil nil nil t)
  :config
  (general-setq gcmh-idle-delay 'auto)
  (general-add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)
  (gcmh-mode 1))

(use-package cl-lib-highlight
  :ghook ('emacs-lisp-mode-hook #'cl-lib-highlight-initialize nil nil t)
  :config
  (cl-lib-highlight-warn-cl-initialize))

(use-package highlight-defined
  :ghook 'emacs-lisp-mode-hook)

(use-package org
  :init
  (my/leader
    "mi" #'org-toggle-inline-images)
  :config
  (setq org-startup-indented t
        org-startup-with-inline-images t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-auto-align-tags nil
        org-tags-column 0
        org-agenda-block-separator ?-
        org-image-actual-width '(300))
  (setq org-directory (concat (getenv "HOME") "/Documents/notes/"))
  (my/leader
    "oa" #'org-agenda
    "oc" #'org-capture)
  (setq org-agenda-files (mapcar (lambda (x)
                                   (concat (getenv "HOME") "/Documents/agenda/" x))
                                 '("todo.org")))
  (setq org-capture-templates
        '(("d" "Distraction" entry
           (file+headline (concat org-directory "distractions.org")
                          "Notes")
           "* %?\n%T")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (defun my--org-mode-setup ()
    (display-line-numbers-mode -1)
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (setq-local line-spacing 0.1)
    (setq-local evil-auto-indent nil))

  (general-add-hook 'org-mode-hook #'my--org-mode-setup))

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-dailies-directory "journal/")
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n y" . org-roam-buffer-toggle)))))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda ()
                      (evil-org-mode)
                      (evil-define-key 'normal evil-org-mode-map
                        (kbd "M-o") (evil-org-define-eol-command
                                     org-insert-heading)
                        (kbd "M-t") (evil-org-define-eol-command
                                     org-insert-todo-heading))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
  :after org
  :disabled t
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-superstar
  :after org
  :disabled t
  :hook (org-mode . org-superstar-mode)
  :config
  (general-setq org-superstar-special-todo-items t))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda))

(use-package olivetti
  :init
  (setq olivetti-body-width 0.67)
  (defun my-distraction-free ()
    (interactive)
    (if (not (bound-and-true-p olivetti-mode))
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (olivetti-mode 1))
      (jump-to-register 1)
      (olivetti-mode -1)))
  (my/leader
    "yo" #'my-distraction-free)
  :commands olivetti-mode)

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-trigger 'manual)
  (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
  (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emms
  :defer t
  :init
  (setq emms-directory (concat (getenv "XDG_DATA_HOME") "/emms")
        emms-cache-file (concat (getenv "XDG_CACHE_HOME") "/emms"))
  :config
  (emms-all)
  (emms-default-players)
  (general-define-key
   :states 'normal
   :keymaps 'emms-playlist-mode-map
   "mr" #'emms-toggle-repeat-playlist
   "mp" #'emms-repeat-playlist
   "mi" #'emms-insert-file
   "mt" #'emms-toggle-repeat-track
   "ms" #'emms-playlist-save
   "mm" #'emms-shuffle))

(use-package request)

(use-package restclient
  :custom (restclient-log-request nil))

(use-package graphql
  :after request)

(use-package graphql-mode
  :after graphql)

(use-package graphql-doc
  :after graphql)

(use-package doom-themes
  :after doom-modeline
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-org-config))

(use-package sicp)

(use-package devdocs
  :init
  (setq devdocs-data-dir (concat (getenv "XDG_DATA_HOME") "/devdocs/"))
  (keymap-global-set "C-h D" #'devdocs-lookup)
  :commands (devdocs-lookup devdocs-install devdocs-update-all))

(use-package rmsbolt
  :commands (rmsbolt rmsbolt-starter))

(use-package geiser
  :defer t
  :init
  (setq geiser-autodoc-identifier-format "%s -> %s")
  :config
  (general-define-key
   :keymaps '(override scheme-mode-map geiser-repl-mode-map)
   :states 'normal
   "SPC-c '" #'switch-to-geiser
   "mc" #'geiser-squarify
   "SPC-c i" #'geiser-set-scheme
   "ml" #'geiser-insert-lambda))

(use-package macrostep-geiser
  :hook (geiser-mode . macrostep-geiser-setup)
  :hook (geiser-repl-mode . macrostep-geiser-setup)
  :init
  (general-define-key
   :keymaps '(scheme-mode-map geiser-repl-mode-map)
   :states 'normal
   "SPC-c m" #'macrostep-expand
   "SPC-c M" #'macrostep-geiser-expand-all))

(use-package flycheck-guile
  :after geiser)

(use-package flycheck
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (delq 'new-line flycheck-check-syntax-automatically)
  (setq flycheck-idle-change-delay 1.0)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-delay 0.25)
  (general-define-key
   :states 'normal
   :keymaps 'flycheck-error-list-mode-map
   "j" #'flycheck-error-list-next-error
   "k" #'flycheck-error-list-previous-error
   "RET" #'flycheck-error-list-goto-error)

  (defun +my-flycheck-goto-last-error! ()
    "Visit the last error in the current buffer."
    (interactive)
    (flycheck-first-error (1- (length flycheck-current-errors))))

  (defun +my-flycheck-goto-first-error! ()
    "Visit the first error in the current buffer."
    (interactive)
    (flycheck-first-error 0))

  (general-define-key
   :states 'normal
   :keymaps 'flycheck-mode-map
   "[g" #'flycheck-previous-error
   "]g" #'flycheck-next-error
   "[G" #'+my-flycheck-goto-first-error!
   "]G" #'+my-flycheck-goto-last-error!)

  (global-flycheck-mode))



(keymap-set 'evil-window-map "C-M-V" #'jw-delete-vertical-windows!)
(keymap-set 'evil-window-map "C-M-S" #'jw-delete-horizontal-windows!)

(use-package eshell
  :straight (:type built-in)
  :defer t
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(use-package solaire-mode
  :after doom-themes
  :config
  (solaire-global-mode +1))
