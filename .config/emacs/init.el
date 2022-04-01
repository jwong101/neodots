;;; init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

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

(pixel-scroll-precision-mode 1)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode 1)
(setq ansi-color-for-comint-mode t)
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

(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
(defun my/setup-prog-hooks ()
  (display-line-numbers-mode)
  (electric-pair-local-mode)
  (setq show-trailing-whitespace t))

(dolist (m '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook m #'my/setup-prog-hooks))

(setq-default indent-tabs-mode nil
	      tab-width 4)
(setq-default tab-always-indent nil)
(setq tabify-regexp "^\t* [ \t]+")
(setq-default word-wrap t)
(setq sentence-end-double-space nil)
(setq require-final-newline t)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))

(use-package diminish)

(use-package general
  :demand t)


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
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (general-setq evil-move-cursor-back nil)
  (setq evil-move-beyond-eol t)
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

(general-auto-unbind-keys)

(general-evil-setup)

(general-create-definer my/leader
  :states 'normal
  :keymaps 'override
  :prefix "SPC")

(my/leader
  "u" #'universal-argument
  "j" #'consult-buffer
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

(my/leader
  :states '(normal visual)
  :keymaps 'lsp-mode-map
  "ls" #'consult-lsp-file-symbols
  "lS" #'consult-lsp-symbols
  "ld" #'lsp-ui-peek-find-definitions
  "la" #'lsp-code-actions-at-point
  "l;" #'lsp-find-references
  "lr" #'lsp-rename)

(general-define-key
 :states '(normal visual)
 :keymaps '(override lsp-mode-map)
 "gd" #'lsp-find-definition
 "K" #'lsp-ui-doc-glance)

(general-define-key
 :states '(normal visual insert)
 :keymaps '(override lsp-mode-map)
 "M-k" #'lsp-signature)

(general-define-key
 :states '(insert emacs)
 :keymaps '(override completion-in-region-mode-map)
 "C-j" #'next-line
 "C-k" #'previous-line)

(general-define-key
 :states 'insert
 :keymaps 'override
 "C-a" #'evil-beginning-of-line
 "C-e" #'evil-end-of-line)

(general-define-key
 :states '(visual operator)
 :keymaps 'evil-outer-text-objects-map
 "c" #'evilnc-outer-commenter)

(general-define-key
 :states '(visual operator)
 :keymaps 'evil-inner-text-objects-map
 "c" #'evilnc-inner-commenter)

(my/leader
  :states '(normal visual)
  :keymaps 'json-mode-map
  "mp" #'json-mode-show-path
  "mt" #'json-toggle-boolean
  "md" #'json-mode-kill-path
  "mx" #'json-nullify-sexp
  "ma" #'json-increment-number-at-point
  "me" #'json-decrement-number-at-point
  "mf" #'json-mode-beautify)

(general-nmap "gc" (general-key-dispatch #'evilnc-comment-operator
                     "gc" #'evilnc-comment-or-uncomment-lines))

(general-vmap "gc" #'comment-or-uncomment-region)

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
  (evilnc-default-hotkeys nil nil))

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
  (defun my/recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))
  (setq recentf-filename-handlers
        '(substring-no-properties my/recent-file-truename abbreviate-file-name))
  (setq recentf-auto-cleanup 'never)
  (general-add-hook 'kill-emacs-hook #'recentf-cleanup)
  (general-add-hook '(my/switch-window-hook write-file-functions)
                    (progn (defun my/--recentf-touch-buffer-h ()
                             "Bump file in recent file list if it was switched or written to"
                             (when buffer-file-name
                               (recentf-add-file buffer-file-name))
                               nil)
                             #'my/--recentf-touch-buffer-h)))

(use-package corfu
  :after vertico
  :demand
  :init
  (corfu-global-mode)
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound
and Vertico is not bound"
    (when (and (where-is-internal #'completion-at-point (list (current-local-map)))
               (not (bound-and-true-p vertico--input)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer)
  :bind
  (:map corfu-map
	    ("SPC" . corfu-insert-separator)))

(use-package lsp-ui)

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
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

(use-package all-the-icons
  :if (display-graphic-p))

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
  :commands
  (lsp lsp-deferred)
  :hook
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
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
  (add-hook 'json-mode-local-vars-hook #'lsp))

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
  :config
  (defun my/setup-pyright ()
    (lsp-deferred))
  (add-hook 'python-mode-hook #'my/setup-pyright)
  :after lsp-mode)

(use-package pip-requirements)

(use-package vterm
  :commands vterm-mode
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  (defun my/vterm-hook ()
    (setq-local confirm-kill-processes nil)
    (setq-local hscroll-margin nil))
  (add-hook 'vterm-mode-hook #'my/vterm-hook))

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
    (lsp))
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
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)))

(use-package doom-modeline
  :after all-the-icons
  :init (doom-modeline-mode 1))

(use-package info-colors
  :after info
  :hook (Info-selection . info-colors-fontify-node))

(use-package ibuffer
  :bind ("C-x b" . ibuffer))

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package consult)

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

(use-package info
  :config
  (general-pushnew (expand-file-name "info" user-emacs-directory)
                   Info-additional-directory-list))

(use-package yaml-mode)

(use-package gcmh
  :ghook ('pre-command-hook nil nil nil t)
  :config
  (general-setq gcmh-idle-delay 'auto)
  (general-add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)
  (gcmh-mode 1))

(use-package cl-lib-highlight
  :ghook ('emacs-lisp-mode-hook #'cl-lib-highlight-initialize nil nil t)
  :config (cl-lib-highlight-warn-cl-initialize))

(use-package highlight-defined
  :ghook 'emacs-lisp-mode-hook)

(use-package org
  :config
  (defun my/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (setq-local evil-auto-indent nil))

  (general-add-hook 'org-mode-hook #'my/org-mode-setup))

(use-package org-bullets
  :after org
  :disabled t
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-trigger 'manual)
  (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
  (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
  (load-theme 'doom-acario-dark t)
  (doom-themes-org-config))

(use-package sicp)

(use-package solaire-mode
  :after doom-themes
  :config
  (solaire-global-mode +1))
