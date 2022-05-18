;;; init.el --- my initialization file. -*- outline-regexp: "(use-package"; lexical-binding: t; -*-

;;; Code:
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
(setopt echo-keystrokes 0.1)
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
(require 'jw-lib)
(use-package diminish)

(use-package general
  :demand t)

(eval-and-compile
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-local #'general-setq-local)
  (defalias 'gsetq-default #'general-setq-default))

(use-package magit
  :init
  (setq transient-levels-file (concat user-emacs-directory "transient/levels")
        transient-values-file (concat user-emacs-directory "transient/values")
        transient-history-file (concat user-emacs-directory "transient/history"))
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t
        magit-save-repository-buffers nil)
  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  (jw-defadvice! +magit--revert-repo-buffers-a (&rest _)
    "Invalidate projectile cache."
    (projectile-invalidate-cache nil))
  (setq magit-credential-cache-daemon-socket (expand-file-name "git/credential/socket" "~/.cache/"))
  (setq transient-display-buffer-action '(display-buffer-below-selected)
        magit-bury-buffer-function #'magit-mode-quit-window)
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  :general
  ('normal dired-mode-map "C" #'magit-clone)
  (my/leader
    "dd" #'magit-file-dispatch
    "dD" #'magit-dispatch)
  :bind (("C-x g" . magit-status))
  ("C-x C-g" . magit-status))

(use-package forge
  :after magit
  :commands forge-create-pullreq forge-create-issue
  :preface
  (setq forge-add-default-bindings nil)
  :config
  (general-def 'normal forge-topic-list-mode-map
    "q" #'kill-current-buffer))

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
  (setq evil-symbol-word-search t)
  (setq evil-ex-visual-char-range t)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-ex-search-persistent-highlight nil)
  (setq evil-ex-interactive-search-highlight 'selected-window)
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-C-u-scroll t)
  (evil-kbd-macro-suppress-motion-error t)
  :config
  (evil-select-search-module 'evil-search-module #'evil-search)
  (advice-add #'evil-visual-update-x-selection :override #'ignore)
  (jw-add-hook! after-change-major-mode-hook
    (defun my--update-evil-shiftwidth ()
      "Update `evil-shift-width' to match `tab-width'."
      (setq-local evil-shift-width tab-width)))
  (defun my--disable-ex-highlights-h ()
    "Disable ex search buffer highlights."
    (interactive)
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)))
  (add-hook 'evil-insert-state-entry-hook #'my--disable-ex-highlights-h)
  (with-eval-after-load 'eldoc
    (eldoc-add-command 'evil-normal-state
                       'evil-insert
                       'evil-change
                       'evil-delete
                       'evil-replace))

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
    (interactive)
    (minibufferp))
  (add-hook 'evil-escape-inhibit-functions #'my--evil-inhibit-escape-in-minibuffer-h))

(declare-function general-auto-unbind-keys "general")
(declare-function general-evil-setup "general")

(general-auto-unbind-keys)

(general-evil-setup)

(declare-function general-add-advice "general")
(declare-function general-nmap "general")

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

(evil-define-motion +evil-my-next-line (count)
  "Move the cursor one screen line down or COUNT lines down."
  :type exclusive
  (let ((line-move-visual (null count)))
    (evil-line-move (or count 1))))

(evil-define-motion +evil-my-prev-line (count)
  "Move the cursor one screen line up or COUNT lines up."
  :type exclusive
  (let ((line-move-visual (null count)))
    (evil-line-move (- (or count 1)))))

(jw-remap! evil-motion-state-map
  'evil-previous-line #'+evil-my-prev-line
  'evil-next-line #'+evil-my-next-line)

(general-nmap "C-l" #'my--disable-ex-highlights-h)


(general-define-key
 :states 'insert
 :keymaps 'override
 "C-a" #'evil-beginning-of-line
 "C-e" #'evil-end-of-line)



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

(cl-pushnew
 (list (rx "*Async Shell Command*" (0+ any)) #'display-buffer-no-window)
 display-buffer-alist
 :test #'equal)

(cl-pushnew
 (list "^\\*Warnings\\*" #'display-buffer-reuse-window)
 display-buffer-alist
 :test #'equal)

(cl-pushnew
 `("^\\*compilation\\*" ,(list #'display-buffer-reuse-window
                               #'display-buffer-in-side-window)
   (slot . -1)
   (side . bottom)
   (window-parameters
    (mode-line-format . none)))
 display-buffer-alist
 :test #'equal)

(general-with 'evil
  (general-add-hook 'after-init-hook
                    (lambda (&rest _)
                      (when-let ((messages-buffer (get-buffer "*Messages*")))
                        (with-current-buffer messages-buffer
                          (evil-normalize-keymaps))))
                    nil
                    nil
                    t))

(minibuffer-depth-indicate-mode)

(general-create-definer my-g-pre
  :states 'normal
  :keymaps 'override
  :prefix "g")

(general-create-definer my-m-pre
  :states 'normal
  :prefix "m")

(with-eval-after-load 'evil
  (keymap-unset evil-normal-state-map "m")
  (keymap-set evil-window-map "p" #'other-window-prefix))

;; (general-define-key
;;  :states '(insert emacs)
;;  :keymaps '(override corfu-map)
;;  "C-h" #'corfu-show-documentation)

(gsetq display-buffer-base-action
       '((display-buffer-reuse-window)
         (reusable-frames . t)))

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
  "Run hooks from `my/switch-buffer-hook' in buffer if it has changed.
ORIG-FN should be buffer switching functions like `switch-to-buffer' or
`display-buffer'. BUFFER-OR-NAME is the buffer to switch to and ARGS
are passed to the next function."
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
  "Run hooks from `my/switch-buffer-hook' in the new buffer.
ORIG-FN is the function we are adding the advice to and
ARGS is self explanatory."
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
  "Run hooks in `my/switch-window-hook'."
  (let ((gc-cons-threshold most-positive-fixnum))
    (unless (or my/--inhibit-switch-window-hooks
                (eq my/--last-window (selected-window))
                (minibufferp))
      (let ((my/--inhibit-switch-window-hooks t))
        (run-hooks 'my/switch-window-hook)
        (setq my/--last-window (selected-window))))))

(declare-function general-add-hook "general")
(general-add-hook 'buffer-list-update-hook #'my/run-switch-window-hooks-h)

(defvar my/switch-frame-hook nil
  "A list of hooks to run after changing the focused frame.")

(defvar my/--inhibit-switch-frame-hooks nil
  "Letvar for inhibiting `my/switch-frame-hook'. Do not set this directly.")

(defvar my/--last-frame nil)

(defun my/run-switch-frame-hooks-h (&rest _)
  "Run hooks in `my/switch-frame-hook'."
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
  :functions evil-goggles-mode
  :init
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  :config
  (evil-goggles-mode))

(general-with 'evil
  (general-def 'normal "M" #'evil-set-marker))

(use-package recentf
  :functions (recentf-cleanup recentf-save-list)
  :init
  (general-add-advice '(after-find-file consult-buffer consult-recent-file)
                      :before
                      (lambda (&rest _)
                        (recentf-mode) t)
                      nil t)
  :config
  (general-setq recentf-max-saved-items 200)
  (defun my--recent-file-truename (file)
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
        (abbreviate-file-name (file-truename (tramp-file-name-localname file)))
      file))
  (add-to-list 'recentf-filename-handlers #'my--recent-file-truename)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)
  (gsetq recentf-auto-cleanup 'never)
  (general-add-hook 'kill-emacs-hook #'recentf-cleanup)
  (general-add-hook '(my/switch-window-hook write-file-functions)
                    (progn (defun my--recentf-touch-buffer-h ()
                             "Bump file in recent file list if it was switched or written to"
                             (when buffer-file-name
                               (recentf-add-file buffer-file-name))
                               nil)
                           #'my--recentf-touch-buffer-h))
  (require 'jw-lib)
  (advice-add 'recentf-load-list :around #'jw-shutup-a)
  (jw-run-with-interval 300 10
                        (jw-shutup
                         (when recentf-mode
                           (recentf-save-list))))
  (jw-add-hook! dired-mode-hook
    (defun my--recentf-add-dired-directory-h ()
      "Add dired directories to recentf file list."
      (recentf-add-file default-directory))))

(setq find-file-suppress-same-file-warnings t)
(gsetq vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                    vc-ignore-dir-regexp
                                    tramp-file-name-regexp
                                    "[/\\\\]node_modules"))

(defvar my-projectile-cache-limit 10000
  "Max number of files in projectile cache.")

(defvar my-projectile-cache-blacklist '("~" "/tmp" "/" "~/.config/emacs/straight/repos"))

(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects
             projectile-switch-project
             projectile-run-async-shell-command-in-root
             projectile-run-eshell
             projectile-dired)
  :init
  (setq projectile-cache-file (concat my-emacs-cache-dir "projectile.cache")
        projectile-auto-discover nil
        projectile-globally-ignored-files '("TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-ignored-projects '("~/")
        projectile-mode-line-function (lambda ()
                                        (if (file-remote-p default-directory) ""
                                          (projectile-default-mode-line))))
  (jw-global-remap!
      'evil-jump-to-tag #'projectile-find-tag
      'find-tag #'projectile-find-tag)
  (my/leader
    "pp" #'projectile-switch-project
    "po" #'projectile-switch-open-project
    "pv" #'projectile-run-vterm
    "pa" #'projectile-run-async-shell-command-in-root
    "pc" #'projectile-compile-project
    "pr" #'projectile-run-project
    "pe" #'projectile-run-eshell
    "pd" #'projectile-dired
    "pk" #'projectile-kill-buffers
    "pg" #'magit-project-status)
  :config
  (projectile-mode +1)
  (setq projectile-project-root-files-bottom-up '(".projectile" ".git")
        projectile-project-root-files '("dune-project" "Cargo.toml" "package.json" "pyproject.toml" "requirements.txt" "setup.py")
        projectile-project-root-files-top-down-recurring '("compile_commands.json" "Makefile"))
  (jw-pushnew! projectile-globally-ignored-directories
    "^_opam$"
    "^node_modules$"
    "^\\.mypy_cache$"
    "^\\.venv$")
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)

  (jw-add-hook! kill-emacs-hook
    (defun +projectile--cleanup-project-cache-h ()
      "Cleanup the projectile cache."
      (when (and (bound-and-true-p projectile-projects-cache)
                 projectile-enable-caching)
        (setq projectile-known-projects
              (cl-remove-if #'projectile-ignored-project-p
                            projectile-known-projects))
        (projectile-cleanup-known-projects)
        (cl-loop with blacklist = (mapcar #'file-truename my-projectile-cache-blacklist)
                 for proot in (hash-table-keys projectile-projects-cache)
                 if (or (not (stringp proot))
                        (string-empty-p proot)
                        (>= (length (gethash proot projectile-projects-cache))
                            my-projectile-cache-limit)
                        (member (substring proot 0 -1) blacklist)
                        (projectile-ignored-project-p proot))
                 do (remhash proot projectile-projects-cache)
                 and do (remhash proot projectile-projects-cache-time)
                 and do (remhash proot projectile-project-type-cache))
        (projectile-serialize-cache))))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))


(use-package find-file-in-project
  :commands (find-file-in-project
             find-file-in-project-by-selected
             find-file-in-project-at-point
             ffip-create-project-file)
  :init
  (setq ffip-use-rust-fd t)
  (setq ffip-project-file '(".projectile" ".git"))
  (setq ffip-project-root-function #'projectile-project-root)
  (jw-global-remap!
      'project-find-file #'find-file-in-project)
  (keymap-set evil-normal-state-map "g f" #'find-file-in-project-at-point)
  (defun +find-file-in-project--call-other-window ()
    "Wrapper for calling `find-file-in-project-at-point' with a prefix argument."
    (interactive)
    (funcall #'find-file-in-project-at-point t))
  (keymap-set evil-window-map "g f" #'+find-file-in-project--call-other-window)
  (my/leader
    "pf" #'find-file-in-project-by-selected
    "pF" #'find-file-in-project
    "pL" #'ffip-create-project-file
    "pr" #'ffip-find-files-resume))

(general-with-package 'compile
  (gsetq compilation-always-kill t
         compilation-ask-about-save nil
         compilation-scroll-output 'first-error))

(use-package ansi-color
  :after compile
  :straight nil
  :config
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(general-with 'debug
  (general-def 'normal debugger-mode-map
    "q" #'debugger-quit)
  (general-after 'evil
    (general-add-hook 'debugger-mode-hook #'evil-normalize-keymaps)))

(use-package corfu
  :after vertico
  :straight (corfu :host github :repo "minad/corfu" :files ("*.el" "extensions/*.el"))
  :functions (global-corfu-mode corfu-insert corfu-mode)
  :demand
  :init
  (setq corfu-cycle t)
  (global-corfu-mode)
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound
and Vertico is not bound."
    (when (and (where-is-internal #'completion-at-point (list (current-local-map)))
               (not (bound-and-true-p vertico--input)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer)
  (general-define-key
   :keymaps '(completion-in-region-mode-map corfu-map)
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
  (keymap-set corfu-map "M-RET" #'+my--corfu-insert-send)
  :bind
  (:map corfu-map
	    ("SPC" . corfu-insert-separator)))

(use-package corfu-history
  :straight nil
  :after corfu
  :config
  (setq corfu-history-length 500)
  (corfu-history-mode))

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
  :functions kind-icon-margin-formatter
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :after lsp-mode
  :init
  (jw-defadvice! +lsp--use-hook-for-ui-a (fn &rest args)
    "Change `lsp--auto-configure' to not autostart `lsp-ui-mode'.
FN is the `lsp--auto-configure' and ARGS are the arguments."
    :around #'lsp--auto-configure
    (cl-letf (((symbol-function 'lsp-ui-mode) #'ignore))
      (apply fn args)))
  :config
  (setq lsp-ui-peek-enable t
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72
        lsp-ui-doc-delay 0.75
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)
  (cl-loop for (key . func) in '(("j" . lsp-ui-peek--select-next)
                                 ("k" . lsp-ui-peek--select-prev)
                                 ("C-k" . lsp-ui-peek--select-prev-file)
                                 ("C-j" . lsp-ui-peek--select-next-file))
           do (keymap-set lsp-ui-peek-mode-map key func))
  (general-def 'normal lsp-ui-mode-map
    :prefix "SPC SPC l"
    "d" #'lsp-ui-peek-find-definitions
    "i" #'lsp-ui-peek-find-implementation
    "r" #'lsp-ui-peek-find-references))

(use-package tablist)

(use-package pdf-tools
  :after tablist
  :straight (pdf-tools :type git :host github :repo "vedang/pdf-tools")
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (defun my--pdf-supress-large-file-prompts-a (_size _op-type filename &optional _offer-raw)
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

(use-package nxml-mode
  :straight (:type built-in)
  :mode "\\.p\\(?:list\\|om\\)\\'"
  :mode "\\.xs\\(?:d\\|lt\\)\\'"
  :mode "\\.rss\\'"
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t)
  (jw-setq-hook! nxml-mode-hook
    tab-width nxml-child-indent))

(use-package csv-mode
  :config
  (my/leader
    :keymaps 'csv-mode-map
    "ma" #'csv-align-fields
    "mu" #'csv-unalign-fields
    "ms" #'csv-sort-fields
    "mS" #'csv-sort-numeric-fields
    "mt" #'csv-transpose
    "mk" #'csv-kill-fields))

(use-package saveplace
  :config
  (defun my--dont-prettify-saveplace-cache-a (fn)
    "Don't prettify the cache."
    (cl-letf (((symbol-function 'pp) #'prin1))
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

(defvar +cargo-asm-buffer+ "*cargo-asm*")

;; (defun +rustic-run-cargo-show-asm ()
;;   "Run cargo asm."
;;   (interactive)
;;   (rustic-run-cargo-command "asm"
;;                             :buffer "*rustic-cargo-asm*"
;;                             :process "rustic-cargo-asm-process"))

(defun +rustic-cargo-asm ()
  "TODO: Call cargo-asm."
  (interactive)
  (let ((buffer (get-buffer-create +cargo-asm-buffer+)))
    (call-process "cargo-asm" nil (cons buffer nil) nil "--help")
    (display-buffer buffer t)))

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :preface
  (setq rust-prettify-symbols-alist nil)
  (with-eval-after-load 'rustic-flycheck
    (remove-hook 'rustic-mode-hook #'flycheck-mode)
    (remove-hook 'rustic-mode-hook #'flymake-mode-off))
  :functions (rustic-cargo-build
               rustic-cargo-check
               rustic-cargo-outdated
               rustic-cargo-fmt
               rustic-cargo-run
               rustic-cargo-bench
               rustic-cargo-current-test
               rustic-cargo-test)
  :config

  (setf (alist-get
         (rx bos (literal +cargo-asm-buffer+) eos)
         display-buffer-alist nil nil #'equal)
        (list (list #'display-buffer-reuse-window
                    #'display-buffer-in-side-window)
              '(window-parameters . ((side . right)))
              '(side . right)))
  (setq rustic-indent-method-chain t)
  (setq rustic-analyzer-command '("~/.local/bin/rust-analyzer"))
  (dolist (match '("^\\*rustic-compilation" "*cargo-run*"))
    (add-to-list 'display-buffer-alist `(,match
                                         ,(list #'display-buffer-reuse-window
                                                #'display-buffer-in-side-window)
                                         (slot . -1)
                                         (side . bottom)
                                         (window-parameters
                                          (mode-line-format . none)))))
  (jw-defadvice! +rustic--dont-install-packages-a (&rest _)
    "Don't install lsp-mode if not available."
    :override #'rustic-install-lsp-client-p
    (message "No LSP server running"))
  (my/leader
    :keymaps 'rustic-mode-map
    "ma" #'rustic-cargo-build
    "mc" #'rustic-cargo-check
    "mf" #'rustic-cargo-fmt
    "mh" #'rustic-doc-search
    "mH" #'rustic-doc-dumb-search
    "mo" #'rustic-cargo-outdated
    "mr" #'rustic-cargo-run
	"mB" #'rustic-cargo-bench
    "mt" #'rustic-cargo-current-test
    "SPC mt" #'rustic-cargo-test))

(use-package orderless
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?' . orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun +orderless--dispatch (pattern index _total)
    "TODO."
    (cond
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ((and
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x200000-\x300000]*$")))
     ((string= "!" pattern) `(orderless-literal . ""))
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref
                            pattern
                            (1- (length pattern)))
                           +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism))))
  (setq orderless-component-separator #'orderless-escapable-split-on-space
        orderless-style-dispatchers '(+orderless--dispatch))
  (set-face-attribute 'completions-first-difference nil :inherit nil))

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
  (setq cape-file-directory-must-exist nil
        cape-dabbrev-min-length 3)

  ;; (jw-add-hook! comint-mode-hook
  ;;   (defun +capf--add-cape-history ()
  ;;     "Add `cape-history' to capf in comint-mode buffers."
  ;;     (unless (seq-contains-p 'completion-at-point-functions #'cape-history)
  ;;       (setq-local completion-at-point-functions )
  ;;       (add-hook 'completion-at-point-functions #'cape-history 0 t))))

  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'dabbrev-capf)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package tempel
  :after evil
  :functions (tempel-expand)
  :bind (:map evil-insert-state-map
              ("C-x C-SPC" . tempel-complete)
              ("C-x C-m" . tempel-insert))
  :init
  (setq tempel-path (concat user-emacs-directory "templates/*"))
  (defun my--tempel-setup-capf ()
    "Add `tempel-expand' to `completion-at-point-functions'."
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

(use-package makefile-executor
  :general
  (my/leader
    "pm" #'makefile-executor-execute-project-target
    "SPC pm" #'makefile-executor-execute-last))

(with-eval-after-load 'make-mode
  (jw-add-hook +make-mode--setup-indent-h ()
    ('makefile-mode-hook :depth 'append)
    (funcall #'jw-setq-tab-width! 8 t)))


(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :init
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-recursive-copies 'always
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask)
  (setq image-dired-dir (concat my-emacs-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        image-dired-thumb-size 150)
  :config
  (defun my--dired-no-revert-in-virtual-buffers-a (&rest args)
    "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')"
    (eq revert-buffer-function #'dired-virtual-revert))

  (advice-add #'dired-buffer-stale-p
              :before-until
              #'my--dired-no-revert-in-virtual-buffers-a)
  (my/leader
    "qp" (jw-quickmark "~/projects")
    "qr" (jw-quickmark "~/repos")
    "qo" (jw-quickmark "~/Documents/notes")
    "qb" (jw-quickmark "~/books")
    "qc" (jw-quickmark "~/.config")
    "qt" (jw-quickmark "~/test-workspace"))
  (general-def 'normal dired-mode-map
    "cd" #'dired-create-directory
    "cc" #'dired-do-rename
    "cm" #'dired-do-chmod
    "ch" #'dired-do-chown
    "cg" #'dired-do-chgrp
    "o" #'dired-do-shell-command))

(use-package wdired
  :straight (:type built-in)
  :after dired
  :general ('normal dired-mode-map "w" #'wdired-change-to-wdired-mode)
  :config
  (setq wdired-create-parent-directories t
        wdired-allow-to-change-permissions t)
  (general-def 'normal wdired-mode-map
    "x" #'wdired-toggle-bit
    "RET" #'wdired-finish-edit))

(use-package dired-x
  :straight (:type built-in)
  :defines (dired-clean-confirm-killing-deleted-buffers)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil)
  (setq dired-clean-confirm-killing-deleted-buffers nil))

(use-package dired-atool
  :general
  ('normal
   dired-mode-map
   "ux" #'dired-atool-do-unpack-with-subdirectory
   "uX" #'dired-atool-do-unpack
   "up" #'dired-atool-do-pack)
  :config
  (cl-pushnew
   (list (rx "*dired-atool:" (0+ any)) #'display-buffer-no-window)
   display-buffer-alist
   :test #'equal))

(use-package dired-avfs
  :after 'dired)

(use-package dired-collapse
  :general
  ('normal
   dired-mode-map
   "zc" #'dired-collapse-mode))

(use-package dired-rsync
  :general (dired-mode-map "C-c C-r" #'dired-rsync))

(use-package fd-dired
  :defer t
  :commands (fd-dired fd-grep-dired fd-name-dired)
  :init
  (jw-global-remap! 'find-dired #'fd-dired)
  (my/leader
    "ff" #'fd-dired
    "fg" #'fd-grep-dired
    "fn" #'fd-name-dired))

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

(with-eval-after-load 'shortdoc
  (cl-pushnew
   (list '(major-mode . shortdoc-mode)
          (list #'display-buffer-reuse-mode-window #'display-buffer-in-side-window)
         '((mode . shortdoc)
           (window-height . 0.3)
           (side . left)
           (slot . 1)))
   display-buffer-alist
   :test #'equal))

(use-package lsp-mode
  :functions (lsp-find-definition
              lsp-signature-toggle-full-docs
              lsp-code-actions-at-point
              lsp-find-references
              lsp-rename)
  :commands
  (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none)
  :init
  (defun +orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun +lsp-mode--setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    (add-hook 'orderless-style-dispatchers #'+orderless-dispatch-flex-first nil t)
    (setq-local completion-at-point-functions
                `(,(cape-capf-buster #'lsp-completion-at-point)
                  cape-dabbrev
                  cape-file))
    (setq-local evil-lookup-func #'lsp-describe-thing-at-point))
  :hook
  (lsp-completion-mode . +lsp-mode--setup-completion)
  :config

  (gsetq lsp-keymap-prefix nil)
  (setq lsp-use-plists t)
  (setq lsp-enable-folding nil
        lsp-enable-on-type-formatting nil
        lsp-headerline-breadcrumb-enable nil)

  (my-m-pre lsp-mode-map
    "l" lsp-command-map
    "a" #'lsp-execute-code-action
    "v" #'lsp-avy-lens
    "i" #'lsp-goto-implementation
    "D" #'lsp-find-declaration
    "T" #'lsp-find-type-definition
    "x" #'lsp-find-references
    "r" #'lsp-rename
    "R" #'lsp-workspace-restart)

  ;; (general-define-key
  ;;  :states '(normal visual)
  ;;  :keymaps 'lsp-mode-map
  ;;  "K" #'lsp-describe-thing-at-point)

  (jw-defadvice! +lsp--inhibit-during-magit-preview-a (&rest _)
    "Don't call `lsp' when the buffer is opened through `magit-find-file'."
    :before-until #'lsp
    (bound-and-true-p magit-buffer-revision))

  (general-define-key
   :states '(normal insert)
   :keymaps 'lsp-mode-map
   "M-k" #'lsp-signature-toggle-full-docs)


  ;; (jw-add-hook! 'lsp-mode-hook
  ;;   (defun my--setup-lsp-keybinds-h ()
  ;;     "Setup keybinds in lsp mode."
  ;;     (my/leader
  ;;       :states '(normal visual)
  ;;       "ls" #'consult-lsp-file-symbols
  ;;       )))

  (my/leader
    :states '(normal visual)
    :keymaps '(override lsp-mode-map)
    "ls" #'consult-lsp-file-symbols
    "ws" #'consult-lsp-symbols
    "ld" #'lsp-ui-peek-find-definitions
    "la" #'lsp-code-actions-at-point
    "l;" #'lsp-find-references
    "rn" #'lsp-rename)
  :hook
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (haskell-literate-mode . lsp-deferred)))


;; Adapted from https://github.com/doomemacs/doomemacs/blob/master/modules/editor/snippets/autoload/snippets.el#L302
;; except with cl-letf
(defun +snippets--expand-on-region-a (fn &optional no-condition)
  "Fix off-by-one when expanding snippets on an evil visual region.
Also strips whitespace out of selection. Also switches to insert mode. If
`evil-local-mode' isn't enabled, or we're not in visual mode, run FN as is.
NO-CONDITION is the same as in `yas-insert-snippet'."
  (if (not (and (bound-and-true-p evil-local-mode)
                (evil-visual-state-p)))
      (funcall fn no-condition)
    ;; Trim whitespace in selected region, so as not to introduce extra
    ;; whitespace into `yas-selected-text'.
    (evil-visual-select (save-excursion
                          (goto-char evil-visual-beginning)
                          (skip-chars-forward " \t")
                          (point))
                        (save-excursion
                          (goto-char evil-visual-end)
                          (skip-chars-backward " \t")
                          (point))
                        'inclusive)
    (cl-letf (((symbol-function 'region-beginning) #'evil-visual-beginning)
              ((symbol-function 'region-end) #'evil-visual-end))
      (funcall fn no-condition)))
  (when (and (bound-and-true-p evil-local-mode)
             (not (or (evil-emacs-state-p)
                      (evil-insert-state-p)))
             (yas-active-snippets))
    (evil-insert-state +1)))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (lsp-mode . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("C-c C-e" . yas-expand))
  :config
  (advice-add 'yas-insert-snippet :around #'+snippets--expand-on-region-a)
  (jw-defadvice! +yas--remove-duplicates-a (templates)
    "Remove duplicates in TEMPLATES."
    :filter-return #'yas--all-templates
    (cl-delete-duplicates templates :test #'equal)))

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
  (add-hook 'json-mode-hook #'lsp-deferred)
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
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-gfm-additional-languages '("sh")
        markdown-nested-imenu-heading-index nil)
  (jw-after! org-src
    (add-to-list 'org-src-lang-modes '("md" . markdown)))
  :config
  (add-to-list 'markdown-code-lang-modes '("rust" . rustic-mode))
  (jw-setq-hook! markdown-mode-hook
    fill-nobreak-predicate (cons #'markdown-code-block-at-point-p
                                 fill-nobreak-predicate)))

(use-package grip-mode
  :general
  (my/leader markdown-mode-command-map 'normal
    "mg" #'grip-mode))

(use-package evil-markdown
  :straight (evil-markdown :host github :repo "Somelauw/evil-markdown")
  :hook (markdown-mode . evil-markdown-mode)
  :config
  (add-hook 'evil-collection-markdown-mode #'evil-normalize-keymaps)
  (general-def 'normal evil-markdown-mode-map
    "TAB" #'markdown-cycle
    [backtab] #'markdown-shifttab
    "M-r" #'browse-url-of-file)
  (general-def 'motion evil-markdown-mode-map
    "[h" #'markdown-next-visible-heading
    "]h" #'markdown-previous-visible-heading
    "[p" #'markdown-demote
    "]p" #'markdown-promote
    "[l" #'markdown-previous-link
    "]l" #'markdown-next-link)
  (general-def 'insert evil-markdown-mode-map
    "M-*" #'markdown-insert-list-item
    "M-b" #'markdown-insert-bold
    "M-i" #'markdown-insert-italic
    "M--" #'markdown-insert-hr))

(use-package typescript-mode
  :commands typescript-tsx-mode
  :init
  (add-to-list 'auto-mode-alist
               (cons "\\.tsx\\'" #'typescript-tsx-mode))
  (jw-after! flycheck
    (declare-function flycheck-add-mode "flycheck")
    (dolist (mode '(web-mode typescript-mode typescript-tsx-mode))
      (flycheck-add-mode 'javascript-eslint mode)))
  (jw-add-hook! typescript-tsx-mode-hook
    (defun +typescript--disable-tide-checkers-h ()
      "Disable tide checkers."
      (jw-pushnew! flycheck-disabled-checkers
        'javascript-jshint
        'tsx-tide
        'jsx-tide)))
  :config
  (define-derived-mode typescript-tsx-mode web-mode "Typescript-TSX")
  (jw-after! lsp-mode
    (add-to-list 'lsp--formatting-indent-alist '(typescript-tsx-mode . typescript-indent-level)))
  (autoload 'js2-line-break "js2-mode" nil t)
  (jw-setq-hook! typescript-mode-hook
    comment-line-break-function #'js2-line-break
    typescript-indent-level 2))

(use-package rjsx-mode
  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :mode "\\.pac\\'"
  :interpreter "node"
  :init
  (jw-after! compilation
    (add-to-list 'compilation-error-regexp-alist 'node)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(node
                   "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
                   2 3 4)))
  :config
  (setq js-chain-indent t
        js2-basic-offset 2
        js2-skip-preprocessor-directives t
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-missing-semi-warning nil
        js2-highlight-level 3
        js2-idle-timer-delay)
  (jw-setq-hook! rjsx-mode-hook
    js-switch-indent-offset js2-basic-offset)
  (jw-defadvice! +rjsx--reparse-a (n)
    "Call the js2 parser when N != 1.
`rjsx-electric-gt' won't insert an ending tag if we leave a self-closing tag
too fast."
    :before #'rjsx-electric-gt
    (when (= n 1) (rjsx-maybe-reparse))))

(use-package xref-js2
  :functions xref-js2-xref-backend
  :commands xref-js2-xref-backend
  :init
  (setq xref-js2-search-program 'rg)
  (jw-add-hook! js2-mode-hook
    (defun +js2--setup-xref-h ()
      "Add `xref-js2-xref-backend' to `xref-backend-functions'."
      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package js2-refactor
  :hook ((js2-mode rjsx-mode) . js2-refactor-mode)
  :config
  (add-hook 'js2-refactor-mode-hook #'evil-normalize-keymaps)
  (let ((js2-refactor-mode-map
         (evil-get-auxiliary-keymap js2-refactor-mode-map 'normal t t)))
    (js2r-add-keybindings-with-prefix "C-c r")))

(use-package npm-mode
  :hook ((js-mode typescript-mode) . npm-mode))

(defvar +sh-extra-keywords
  '("cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git" "grep"
    "kill" "less" "ln" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd" "rm"
    "sleep" "sudo" "touch" "tar" "fd" "jq" "curl" "rg" "fd" "gzip")
  "Extra keywords to fontify in `sh-mode'.")

(use-package sh-script
  :straight (:type built-in)
  :mode ("\\.\\(?:zunit\\|env\\)\\'")
  :config
  (add-hook 'sh-mode-hook #'lsp-deferred 'append)
  (setq sh-indent-after-continuation 'always)
  (add-to-list
   'sh-imenu-generic-expression
   '(sh
     (nil
      "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
     (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1)))
  (advice-add #'sh-set-shell :around #'jw-shutup-a)
  (jw-add-hook! sh-mode-hook
    (defun +sh--fontify-extra-keywords ()
      "Fontify `+sh-extra-keywords' in `sh-mode'."
      (font-lock-add-keywords
       nil `(,(regexp-opt +sh-extra-keywords 'symbols)
             (0 'font-lock-type-face append))))))

(use-package lsp-pyright
  :after lsp-mode)

(use-package poetry
  :after python
  :disabled t
  :hook (python-mode . poetry-tracking-mode)
  :init
  (setq poetry-tracking-strategy 'switch-buffer))

(use-package python
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :init
  ;; (gsetq python-shell-interpreter "ipython"
  ;;        python-shell-interpreter-args "-i --simple-prompt --no-color-info")
  (my-m-pre python-mode-map
    "b" #'python-shell-send-buffer
    "d" #'python-shell-send-defun
    ";" #'python-shell-switch-to-shell)
  (setq python-indent-guess-indent-offset-verbose nil)
  (general-def 'visual python-mode-map
    :prefix "m"
    "l" #'python-shell-send-region)
  (jw-defadvice! +python--shell-get-process (&rest _)
    "Return the python process, and start it if necessary."
    :override #'python-shell-get-process-or-error
    (or (python-shell-get-process)
        (progn (run-python)
               (python-shell-get-process))))
  (jw-setq-hook! python-mode-hook
    tab-width python-indent-offset
    flycheck-python-flake8-executable "flake8"))

(use-package pydoc
  :general
  (my-m-pre python-mode-map
    "h" #'pydoc-at-point
    "/" #'pydoc))

(use-package python-docstring
  :ghook 'python-mode-hook)

(use-package sphinx-doc
  :ghook 'python-mode-hook
  :config
  (my-m-pre python-mode-map
    "s" #'sphinx-doc))

(use-package pip-requirements
  :defer t
  :config
  (jw-defadvice! +python--init-completion-a (&rest args)
    "Call `pip-requirements-fetch-packages' the first time
`pip-requirements-complete-at-point' is invoked."
    :before #'pip-requirements-complete-at-point
    (unless pip-packages
      (pip-requirements-fetch-packages)
      (advice-remove 'pip-requirements-complete-at-point #'+python--init-completion-a)))
  (jw-defadvice! +python--inhibit-pip-requirements-fetch-package-a (fn &rest args)
    "Disable `pip-requirements-fetch-packages'."
    :around #'pip-requirements-mode
    (cl-letf (((symbol-function 'pip-requirements-fetch-packages) #'ignore))
      (apply fn args))))

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

(use-package demangle-mode
  :hook llvm-mode)

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

(use-package haskell-mode
  :functions (haskell-indentation-indent-line
               haskell-indentation-newline-and-indent)
  :init
  (setq haskell-process-show-overlays nil
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (defun +haskell/evil-open-above ()
    "Opens a line above."
    (interactive)
    (evil-beginning-of-line)
    (haskell-indentation-newline-and-indent)
    (evil-previous-line)
    (haskell-indentation-indent-line)
    (evil-append-line nil))

  (defun +haskell/evil-open-below ()
    "Opens a line below in haskell mode."
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))

  (general-nmap
    :keymaps 'haskell-mode-map
    "o" #'+haskell/evil-open-below
    "O" #'+haskell/evil-open-above)

  (my/leader
    :states '(normal visual)
    :keymaps 'haskell-mode-map
    "mp" #'haskell-cabal-visit-file
    "ma" #'haskell-process-cabal-build))

(use-package lsp-haskell
  :after lsp-mode
  :init
  (setq lsp-haskell-formatting-provider "stylish-haskell")
  (add-hook 'haskell-mode-hook #'lsp-deferred 'append))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package helpful
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :functions helpful-kill-buffers
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
  (setq help-window-keep-selected t)
  (cl-pushnew
   (list (cons 'major-mode 'helpful-mode) #'display-buffer-reuse-mode-window #'display-buffer-use-least-recent-window)
   display-buffer-alist :test #'equal)
  (defun my--use-helpful-a (fn &rest args)
    "Force FN to use helpful instead of the default."
    (cl-letf (((symbol-function 'describe-function) #'helpful-function)
              ((symbol-function 'describe-variable) #'helpful-variable))
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
  (evil-add-command-properties #'helpful-visit-reference :jump t)
  (defun +helpful-kill-all-buffers ()
    (interactive)
    (kill-matching-buffers "^\\*Help\\*" nil t)
    (helpful-kill-buffers))
  (my/leader
    "hk" #'+helpful-kill-all-buffers)
  (general-def 'normal helpful-mode
    :definer 'minor-mode
    "q" #'quit-window))

(general-def special-mode-map 'normal
  "q" #'quit-window)

(with-eval-after-load 'tramp
  (setq remote-file-name-inhibit-cache 60)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package doom-modeline
  :after all-the-icons
  :functions doom-modeline-mode
  :init (doom-modeline-mode 1))

(use-package info-colors
  :after info
  :hook (Info-selection . info-colors-fontify-node))

(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :general
  (my/leader "=" #'apheleia-format-buffer)
  :init (setq apheleia-hide-log-buffers t)
  :config
  (cl-psetf (alist-get 'isort apheleia-formatters) '("isort" "--stdout" "-")
            (alist-get 'python-mode apheleia-mode-alist) '(isort black)
            (alist-get 'stylish-haskell apheleia-formatters) "stylish-haskell"
            (alist-get 'haskell-mode apheleia-mode-alist) 'stylish-haskell))

(use-package pass)
(use-package password-store
  :config
  (jw-after! embark
    (embark-define-keymap embark-password-store-actions
      "password-store actions keymap."
      ("c" password-store-copy)
      ("f" password-store-copy-field)
      ("i" password-store-insert)
      ("I" password-store-generate)
      ("r" password-store-rename)
      ("e" password-store-edit)
      ("R" password-store-remove)
      ("U" password-store-url))
    (add-to-list 'embark-keymap-alist '(password-store . embark-password-store-actions))
    (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store))))
(use-package password-store-otp)

(declare-function password-store-dir "password-store")

(jw-defadvice! +my--pass-use-dir-env-a (entry)
  "Return a string with the file content of ENTRY."
  :override #'auth-source-pass--read-entry
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (format "%s.gpg" entry) (password-store-dir)))
    (buffer-substring-no-properties (point-min) (point-max))))

(jw-after! pass
  (evil-set-initial-state 'pass-mode 'insert)
  (general-def pass-mode-map
    "j" #'pass-next-entry
    "k" #'pass-prev-entry
    "d" #'pass-kill
    "\C-j" #'pass-next-directory
    "\C-k" #'pass-prev-directory)
  (defun +pass-consult (arg pass)
    "Use consult to store url or copy PASS depending on ARG."
    (interactive
     (list current-prefix-arg
           (progn
             (require 'consult)
             (consult--read (password-store-list)
                            :prompt "Pass: "
                            :sort nil
                            :require-match t
                            :category 'pass))))
    (funcall (if arg
                 #'password-store-url
               #'password-store-copy)
             pass)))

(auth-source-pass-enable)

(use-package ibuffer
  :bind ("C-x b" . ibuffer))

(use-package vertico
  :demand t
  :straight (vertico :type git :host github :repo "minad/vertico" :files ("*.el" "extensions/*.el"))
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :custom
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-count 20)
  :config
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (vertico-mode)
  (my/leader
    "s." #'vertico-repeat)
  (vertico-indexed-mode)
  (keymap-set minibuffer-local-filename-completion-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char))

(use-package consult
  :demand t
  :functions (consult-locate
               consult-apropos
               consult-man
               consult-imenu
               consult-bookmark
               consult-yank-pop)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-local-map
              ("C-r" . consult-history))
  :init
  (advice-add #'multi-occur :override #'consult-multi-occur)
  (jw-global-remap!
   'switch-to-buffer #'consult-buffer
   'switch-to-buffer-other-window #'consult-buffer-other-window
   'switch-to-buffer-other-frame #'consult-buffer-other-frame
   'locate #'consult-locate
   'apropos-command #'consult-apropos
   'man #'consult-man
   'imenu #'consult-imenu
   'evil-show-marks #'consult-mark
   'evil-show-registers #'consult-register
   'goto-line #'consult-goto-line
   'load-theme #'consult-theme
   'bookmark-jump #'consult-bookmark
   'yank-pop #'consult-yank-pop)

  (setq crm-separator "&")

  (my-g-pre
    "m" #'bookmark-set
    "'" #'consult-bookmark)
  (jw-after! projectile
    (jw-remap! projectile-command-map
      'projectile-switch-to-buffer #'consult-project-buffer))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (my/leader
    "pb" #'consult-project-buffer)
  (setq consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (jw-after! projectile
    (setq consult-project-function (lambda (_) (projectile-project-root))))

  (defun +consult-line-at-point ()
    "Call `consult-line' with initial input at point."
    (interactive)
    (funcall #'consult-line (thing-at-point 'symbol t)))

  (my/leader
    "cl" #'consult-line
    "cs" #'+consult-line-at-point
    "co" #'consult-outline
    "cr" #'consult-complex-command
    "hm" #'consult-man
    "bb" #'consult-bookmark)

  (dolist (cmd '(#'consult-imenu
                 #'consult-imenu-multi
                 #'consult-line
                 #'consult-goto-line
                 #'consult-line
                 #'consult-line-multi
                 #'consult-bookmark
                 #'consult-xref
                 #'consult-ripgrep
                 #'consult-org-heading
                 #'consult-outline))
    (evil-add-command-properties cmd :jump t)))


(use-package consult-dir
  :after (consult vertico)
  :commands (consult-dir consult-dir-jump-file)
  :bind (([remap list-directory] . consult-dir)
         :map minibuffer-local-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :init
  (my/leader
    "cd" #'consult-dir)
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package dumb-jump
  :commands dumb-jump-xref-activate
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 'append))

(use-package savehist
  :init
  (savehist-mode))

(use-package all-the-icons-completion)

(use-package marginalia
  :after vertico
  :functions (marginalia-mode)
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (marginalia-mode)
  (general-pushnew '(project-find-file . project-file)
                   marginalia-command-categories)
  (general-pushnew '(find-file-in-project . project-file)
                   marginalia-command-categories)
  (general-pushnew '(project-find-dir . file)
                   marginalia-command-categories)
  (general-pushnew '(project-switch-project . file)
                   marginalia-command-categories)
  (general-pushnew '(flycheck-error-list-set-filter . builtin)
                   marginalia-command-categories)
  (general-pushnew '("Find file:" . file)
                   marginalia-prompt-categories)
  (jw-pushnew! marginalia-command-categories
    '(projectile-find-file . project-file)
    '(projectile-find-dir . file)
    '(projectile-switch-project . project-file)
    '(projectile-recentf . project-file)
    '(projectile-switch-to-buffer . buffer)))

(defun +vertico-magit-status (file)
  "Run `magit-status' on repo containing FILE."
  (interactive "GFile: ")
  (magit-status (locate-dominating-file file ".git")))

(defun jw-sudo-edit ()
  "Edit file with sudo."
  (interactive)
  (find-file (concat "/sudo:root@localhost:"
                     (expand-file-name (read-file-name "Find file as root: ")))))

(use-package embark
  :functions (embark-prefix-help-command embark-bindings)
  :commands (embark-prefix-help-command embark-bindings)
  :bind
  (("C-;" . embark-act)
   :map minibuffer-local-map
   ("C-c C-b" . embark-become)
   ("C-c C-;" . embark-export)
   ("C-c C-l" . embark-collect))
  :init
  (my/leader
    "aa" #'embark-act)
  (setq embark-confirm-act-all nil)
  (setq prefix-help-command #'embark-prefix-help-command)
  (jw-global-remap! #'describe-bindings #'embark-bindings)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (general-def 'normal embark-collect-mode-map
    "w" #'wgrep-change-to-wgrep-mode)
  (general-def embark-file-map
    "s" #'+vertico-magit-status
    "C-e" #'jw-sudo-edit)
  (general-def embark-become-file+buffer-map
    "c" #'projectile-switch-project
    "P" #'find-file-in-project
    "p" #'projectile-find-file-in-known-projects
    "b" #'consult-buffer))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package rainbow-mode)

(use-package consult-lsp
  :after (marginalia consult lsp-mode)
  :init
  (jw-remap! lsp-mode-map 'xref-find-apropos #'consult-lsp-symbols))

(use-package gitignore-mode
  :straight git-modes
  :mode "/.dockerignore\\'")

(use-package gitattributes-mode
  :straight git-modes)

(use-package gitconfig-mode
  :straight git-modes)

(use-package magit-todos
  :after magit
  :functions magit-todos-mode
  :config
  (magit-todos-mode)
  (keymap-set magit-todos-section-map "j" nil))

(use-package git-timemachine
  :general
  (my/leader
    "gt" #'git-timemachine)
  :config
  (general-def 'normal git-timemachine-mode-map
    "b" #'git-timemachine-blame
    "c" #'git-timemachine-show-commit
    "q" #'git-timemachine-quit))

(general-with-package 'compile
  (general-setq compilation-always-kill t
                compilation-ask-about-save nil))

(use-package pkgbuild-mode)

(use-package docker
  :init
  (add-hook 'dockerfile-mode-hook #'lsp-deferred 'append)
  :custom (docker-image-run-arguments '("-i" "-t" "--rm"))
  :commands (docker))

(use-package docker-tramp)

(use-package dockerfile-mode)

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package info)
  ;; :config
  ;; (general-pushnew (expand-file-name "info" user-emacs-directory)
                   ;; Info-additional-directory-list))

(use-package elisp-demos
  :defer t
  :commands (elisp-demos-advice-describe-function-1 elisp-demos-advice-helpful-update)
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package yaml-mode
  :init
  (add-hook 'yaml-mode-hook #'lsp-deferred 'append)
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
  :functions (lispyville-set-key-theme)
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
  :functions cl-lib-highlight-warn-cl-initialize
  :ghook ('emacs-lisp-mode-hook #'cl-lib-highlight-initialize nil nil t)
  :config
  (cl-lib-highlight-warn-cl-initialize))

(use-package highlight-defined
  :ghook 'emacs-lisp-mode-hook)

(use-package org
  :functions (org-toggle-inline-images)
  :preface
  (setq org-publish-timestamp-directory (concat my-emacs-cache-dir "org-timestamps/")
        org-preview-latex-image-directory (concat my-emacs-cache-dir "org-latex/")
        org-list-allow-alphabetical t)
  :init
  (my/leader org-mode-map
    "mi" #'org-toggle-inline-images)
  (setq org-enforce-todo-dependencies t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t)
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
  (setq org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-src-window-setup 'other-window)

  (add-to-list 'org-export-backends 'md)

  ;; org hacks
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  (jw-pushnew! org-file-apps
    '(directory . emacs)
    '(remote . emacs))

  (define-advice org-fix-tags-on-the-fly (:before-while
                                          (&rest _)
                                          +org--respect-auto-align-a)
    "Don't align tags if `org-auto-align-tags' is set."
    org-auto-align-tags)

  (define-advice org-return (:after
                             (&optional indent _arg _interactive)
                             +org--fix-indent-in-src-block-a)
    "Mimic `newline-and-indent' in src blocks."
    (when (and indent
               org-src-tab-acts-natively
               (org-in-src-block-p t))
      (save-window-excursion
        (org-babel-do-in-edit-buffer
         (call-interactively #'indent-for-tab-command)))))

  (add-hook 'org-after-refile-insert-hook #'save-buffer)

  (keymap-set org-src-mode-map "C-c C-c" #'org-edit-src-exit)
  (setq org-directory (expand-file-name "~/Documents/notes/")
        org-default-notes-file (expand-file-name "default-notes.org" org-directory))
  (my/leader
    "oa" #'org-agenda
    "oc" #'org-capture)
  (setq org-agenda-files (mapcar (lambda (x)
                                   (concat (getenv "HOME") "/Documents/agenda/" x))
                                 '("todo.org")))
  (defvar +org-main-gtd-file (expand-file-name "~/Documents/notes/inbox.org"))
  (setq org-capture-templates
        `(("d" "Distraction" entry
           (file+headline ,(concat org-directory "distractions.org")
                          "Notes")
           "* Distraction %?\n %i\n %a")
          ("t" "Todo" entry
           (file+headline +org-main-gtd-file "Tasks")
           "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (defun my--org-mode-setup ()
    (display-line-numbers-mode -1)
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (setq-local tab-width org-indent-indentation-per-level)
    (setq-local evil-shift-width org-indent-indentation-per-level)
    (setq-local line-spacing 0.1)
    (setq-local evil-auto-indent nil))

  (advice-add #'org-link--open-help :around #'my--use-helpful-a)

  (jw-add-hook! org-agenda-finalize-hook
    (defun +org--exclude-from-perspective-h ()
      "Don't add org agenda buffers to perspective."
      (when (and org-agenda-new-buffers
                 (bound-and-true-p persp-mode)
                 (not org-agenda-sticky))
        (dolist (buf org-agenda-new-buffers)
          (persp-forget-buffer buf)))))

  (define-advice org-get-agenda-file-buffer (:around (fn file)
                                                     +org-agenda--ignore-recentf-a)
    "Don't add org agenda buffers to recentf."
    (let ((recentf-exclude (list (lambda (_file) t)))
          org-mode-hook
          vc-handled-backends
          find-file-hook)
      (funcall fn file)))

  (jw-defadvice! +org--inhibit-save-hooks-a (fn &rest args)
    "Don't trigger save hooks."
    :around '(org-export-to-file org-babel-tangle)
    (let (before-save-hook after-save-hook)
      (apply fn args)))

  (define-keymap :keymap org-mode-map
    "TAB" #'org-cycle
    "C-c C-i" #'org-toggle-inline-images
    "C-M-RET" #'org-insert-subheading)

  (general-def '(normal visual insert) org-mode-map
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m"
    "'" #'org-edit-special
    "*" #'org-ctrl-c-star
    "-" #'org-ctrl-c-minus
    "," #'org-switchb
    "." #'consult-org-heading
    "/" #'consult-org-agenda
    "A" #'org-archive-subtree
    "e" #'org-export-dispatch
    "f" #'org-footnote-action
    "n" #'org-store-link
    "h" #'org-toggle-heading
    "i" #'org-toggle-item
    "I" #'org-id-get-create
    "t" #'org-todo
    "T" #'org-todo-list
    "x" #'org-toggle-checkbox
    "@" #'org-cite-insert
    "lc" #'org-cliplink
    "ll" #'org-insert-link
    "lL" #'org-insert-all-links
    "ls" #'org-store-link
    "lt" #'org-toggle-link-display
    "sa" #'org-toggle-archive-tag
    "sb" #'org-tree-to-indirect-buffer
    "sd" #'org-cut-subtree
    "sh" #'org-promote-subtree)

  (general-def '(normal visual insert) org-mode-map
    :prefix "SPC a"
    :non-normal-prefix "M-SPC a"
    "a" #'org-attach
    "d" #'org-attach-delete-one
    "D" #'org-attach-delete-all
    "n" #'org-attach-new
    "o" #'org-attach-open
    "r" #'org-attach-reveal
    "u" #'org-attach-url
    "s" #'org-attach-set-directory
    "S" #'org-attach-sync)

  (setq org-id-locations-file-relative t)
  (jw-defadvice! +org-id--fail-gracefully-a (&rest _)
    "Check if file is writeable before trying to write to it."
    :before-while '(org-id-locations-save org-id-locations-load)
    (file-writable-p org-id-locations-file))


  (general-add-hook 'org-mode-hook #'my--org-mode-setup))


(defun +org-image-file-data-fn (protocol link _description)
  "Interpret LINK as an image file path and return its data.
PROTOCOL is either \"download\" or \"attachment\"."
  (setq link (expand-file-name
              link
              (pcase protocol
                ("download"
                 (require 'org-download)
                 org-download-image-dir)
                ("attachment"
                 (require 'org-attach)
                 org-attach-id-dir)
                (_ default-directory))))
  (when (and (file-exists-p link)
             (image-supported-file-p link))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq-local buffer-file-coding-system 'binary)
      (insert-file-contents-literally link)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun +org-inline-image-data-fn (_protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (base64-decode-string link))

(jw-after! ox
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-latex-prefer-user-labels t))

(jw-after! org-capture
  (org-capture-put :kill-buffer t))

(use-package citar
  :init
  (setq citar-at-point-function 'embark-act
        citar-library-paths `(,(expand-file-name "~/books/references")))
  (jw-after! all-the-icons
    (setq citar-symbols
          `((file ,(all-the-icons-faicon "file-o"
                                         :face 'all-the-icons-green
                                         :v-adjust -0.1) . " ")
            (note ,(all-the-icons-material "speaker_notes"
                                           :face 'all-the-icons-blue
                                           :v-adjust -0.3) . " ")
            (link ,(all-the-icons-octicon "link"
                                          :face 'all-the-icons-orange
                                          :v-adjust 0.01) . " ")))
    (setq citar-symbol-separator " "))

  (defun +bibtex-key-embark ()
    "Bibtex finder for embark."
    (when-let (not minibuf))
    (save-excursion
      (bibtex-beginning-of-entry)
      (when (looking-at bibtex-entry-maybe-empty-head)
        (cons 'bibtex-key (bibtex-key-in-head)))))

  (jw-after! embark

    ;; (add-to-list 'embark-target-finders #'+bibtex-key-embark)
    (embark-define-keymap bibtex-key-embark-map
      "Embark keymap for bibtex."
      ("f" #'citar-open)
      ("n" #'citar-open-notes))
    (add-to-list 'embark-keymap-alist '(bibtex-key . bibtex-key-embark-map))

    (add-to-list 'embark-keymap-alist '(bib-reference . citar-map))
    (defvar-keymap +embark-become-citar-map
      :doc "Embark become keymap for biblio lookup."
      "f" #'citar-open-library-files
      "x" #'biblio-arxiv-lookup
      "c" #'biblio-crossref-lookup
      "i" #'biblio-ieee-lookup
      "h" #'biblio-hal-lookup
      "s" #'biblio-dissemin-lookup
      "b" #'biblio-dblp-lookup
      "o" #'biblio-doi-insert-bibtex)
    (add-to-list 'embark-become-keymaps '+embark-become-citar-map))

  :config
  (defun citar--add-file-to-library (key)
    "Add a file to the library for KEY.
The FILE can be added either from an open buffer, a file, or a
URL."
    (let* ((source
            (char-to-string
             (read-char-choice
              "Add file from [b]uffer, [f]ile, or [u]rl? " '(?b ?f ?u))))
           (directory (if (cdr citar-library-paths)
                          (completing-read "Directory: " citar-library-paths)
                        (car citar-library-paths)))
           (file-path
            (file-name-concat directory (concat key ".pdf")))) ; FIX so don't hardcode extension
      (pcase source
        ("b"
         (with-current-buffer (read-buffer-to-switch "Add file buffer: ")
           (write-file file-path)))
        ("f"
         (copy-file
          (expand-file-name
           (read-file-name "Add file: " nil nil t)) file-path))
        ("u"
         (url-copy-file (read-string "Add file URL: ") file-path)))))

  (defun +citar-add-file-to-library (key-entry)
    "Add a file to the library for KEY-ENTRY.
The FILE can be added either from an open buffer, a file, or a
URL."
    (interactive (list (citar-select-ref
                        :rebuild-cache current-prefix-arg)))
    (citar--add-file-to-library (car key-entry)))
  (my-g-pre
    "l" #'citar-open)
  (my/leader
    "cC" #'+citar-add-file-to-library)
  :custom
  (org-cite-global-bibliography '("~/Documents/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("C-c c p" . citar-insert-preset)))

(use-package org-attach
  :straight nil
  :after org
  :commands (org-attach-new
             org-attach-open
             org-attach-open-in-emacs
             org-attach-reveal-in-emacs
             org-attach-url
             org-attach-set-directory
             org-attach-sync)
  :config
  (unless org-attach-id-dir
    (setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory)))
  (jw-after! projectile
    (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir))
  (org-link-set-parameters "attachment" :image-data-fun #'+org-inline-image-data-fn))

(use-package org-roam
  :after org
  :defines (org-roam-v2-ack org-roam-dailies-directory)
  :functions org-roam-db-autosync-enable
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-dailies-directory "journal/")
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-db-autosync-enable)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n y" . org-roam-buffer-toggle)))))

(use-package consult-org-roam
  :after org-roam
  :demand t
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :config
  (consult-org-roam-mode 1)
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind (("C-c n e" . consult-org-roam-file-find)
         ("C-c n b" . consult-org-roam-backlinks)
         ("C-c n s" . consult-org-roam-search)))

;;TODO: check out org-super-agenda and org-ql

(use-package evil-org
  :after org
  :functions (evil-org-mode evil-org-agenda-set-keys)
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row))
  (defvar evil-org-use-additional-insert t)
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme)
  (general-def 'motion evil-org-mode-map
    "]h" #'org-forward-heading-same-level
    "[h" #'org-backward-heading-same-level
    "]l" #'org-next-link
    "[l" #'org-previous-link
    "]c" #'org-babel-next-src-block
    "[c" #'org-babel-previous-src-block)
  (general-def 'normal evil-org-mode-map
    "zA" #'org-shifttab
    "zC" #'outline-hide-subtree
    "zn" #'org-tree-to-indirect-buffer
    "gQ" #'org-fill-paragraph
    "zO" #'outline-show-subtree
    "zi" #'org-toggle-inline-images)
  (general-def '(normal insert) evil-org-mode-map
    "C-S-l" #'org-shiftright
    "C-S-h" #'org-shiftleft
    "C-S-k" #'org-shiftup
    "C-S-j" #'org-shiftdown))

(use-package evil-org-agenda
  :straight nil
  :hook (org-agenda-mode . evil-org-agenda-mode)
  :config
  (evil-org-agenda-set-keys)
  (evil-define-key* 'motion evil-org-agenda-mode-map
    (kbd "SPC") nil))

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

(use-package org-contrib
  :after org
  :straight (org-contrib :host github :repo "emacsmirror/org-contrib"))

(use-package ox-pandoc
  :after ox
  :init
  (add-to-list 'org-export-backends 'pandoc))

(use-package htmlize)

(use-package ox-clip
  :after ox)

(use-package org-cliplink
  :after org
  :general
  (my/leader
    "ol" #'org-cliplink))

(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable)
  :config
  (define-advice toc-org-insert-toc (:around
                                     (fn &rest args)
                                     +org-toc-inhibit-scrolling)
    "Prevent scrolling that occurs when the TOC is regenerated."
    (let ((p (set-marker (make-marker) (point)))
          (s (window-start)))
      (prog1
          (apply fn args)
        (goto-char p)
        (set-window-start nil s t)
        (set-marker p nil)))))

(jw-after! org-eldoc
  (puthash "org" #'ignore org-eldoc-local-functions-cache)
  (puthash "plantuml" #'ignore org-eldoc-local-functions-cache)
  (puthash "python" #'python-eldoc-function org-eldoc-local-functions-cache))

(use-package org-yt
  :after org
  :straight (org-yt :host github :repo "TobiasZawada/org-yt"))

(use-package ob-async)

(use-package ob-graphql
  :defer t
  :after org)

(use-package ob-restclient
  :defer t
  :after org)

(use-package orgit
  :after (magit org))

(use-package orgit-forge
  :after orgit)

(use-package org-pdftools
  :after (org pdftools)
  :commands org-pdftools-export
  :init
  (jw-after! org
    (jw-add-hook! org-open-link-functions
      (defun +org--open-legacy-pdf-links-h (link)
        "Open pdftools:* and pdfviews:* links as if they were pdf:* links."
        (let ((regexp "^pdf\\(?:tools\\|view\\):"))
          (when (string-match-p regexp link)
            (org-pdftools-open (replace-regexp-in-string regexp "" link))
            t))))))

(use-package org-noter
  :after org)

(use-package org-download
  :after org)

(use-package org-journal
  :after org)

(use-package org-brain
  :after org)

(use-package gnuplot
  :defer t)
(use-package gnuplot-mode
  :defer t)

(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

(defun my/org-mode-visual-fill ()
  "Setup `visual-fill-column-mode' in `org-mode'."
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emms
  :defer t
  :functions (emms-all emms-default-players)
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

(use-package restclient-jq
  :straight (restclient-jq :type git
                           :host github
                           :repo "pashky/restclient.el"
                           :files ("restclient-jq.el"))
  :after restclient)

(use-package jq-mode
  :after restclient-jq)

(use-package graphql
  :after request)

(use-package graphql-mode
  :after graphql)

(use-package graphql-doc
  :after graphql)

(use-package doom-themes
  :after doom-modeline
  :functions (doom-themes-org-config)
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
  :hook ((prog-mode text-mode conf-mode) . flycheck-mode)
  :config
  (declare-function flycheck-first-error "flycheck")
  (declare-function flycheck-next-error "flycheck")
  (declare-function flycheck-previous-error "flycheck")
  (declare-function flycheck-error-list-next-error "flycheck")
  (declare-function flycheck-error-list-previous-error "flycheck")
  (declare-function flycheck-error-list-goto-error "flycheck")
  (declare-function global-flycheck-mode "flycheck")
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

  (setq flycheck-markdown-markdownlint-cli-executable "markdownlint-cli2"))


(with-eval-after-load 'project
  (add-to-list 'project-switch-commands '(magit "Magit Status" ?m)))

;;TODO: use dumb-jump as the main fallback in all prog modes
;;and setup some macros. Also investigate integrating this jumps w/
;;gumshoe and tracking them in the taglist and jumplist in `evil-mode'.
(use-package smart-jump
  :commands (smart-jump-go smart-jump-back smart-jump-references)
  :disabled t
  :init
  (jw-after! general
    (general-define-key
     :states '(normal visual motion)
     :keymaps 'override
     "M-." #'smart-jump-go
     "M-," #'smart-jump-back
     "M-?" #'smart-jump-references)))


(declare-function jw-delete-vertical-windows! "jw-lib")
(declare-function jw-delete-horizontal-windows! "jw-lib")
(keymap-set evil-window-map "C-M-V" #'jw-delete-vertical-windows!)
(keymap-set evil-window-map "C-M-S" #'jw-delete-horizontal-windows!)
(keymap-set evil-window-map "e" #'jw-delete-other-window)

(use-package macrostep
  :commands (macrostep-expand)
  :init
  (keymap-set emacs-lisp-mode-map "C-c e" #'macrostep-expand))

(use-package eshell
  :straight (:type built-in)
  :defer t
  :commands eshell
  :init
  (my/leader
    "se" #'eshell)
  :config
  (require '+eshell)
  (setq eshell-hist-ignoredups t
        eshell-kill-processes-on-exit t
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-prompt-fn)
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(use-package eshell-up
  :after eshell
  :commands (eshell-up eshell-up-peek))

(use-package esh-help
  :after eshell
  :functions (setup-esh-help-eldoc)
  :config (setup-esh-help-eldoc))

(use-package dash-docs
  :config
  (setq dash-docs-docsets-path (concat my-emacs-cache-dir "docsets/")
        dash-docs-browser-func #'eww))

(use-package rg
  :functions rg-menu
  :commands rg-menu
  :init
  (my/leader
    "R" #'rg-menu))

(use-package just-mode)

(jw-after! lisp-mode
  (setq emacs-lisp-docstring-fill-column 80))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(setq TeX-parse-self t
      TeX-auto-save t
      TeX-auto-local ".auctex-auto"
      TeX-style-local ".auctex-style"
      TeX-source-correlate-mode t
      TeX-source-correlate-start-server nil
      TeX-electric-sub-and-superscript t
      TeX-save-query nil)

;;from doom's config
(defun +latex-indent-item-fn ()
  "Indent LaTeX \"itemize\",\"enumerate\", and \"description\" environments.
\"\\item\" is indented `LaTeX-indent-level' spaces relative to the the beginning
of the environment.
See `LaTeX-indent-level-item-continuation' for the indentation strategy this
function uses."
  (save-match-data
    (let* ((re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env "\\(?:itemize\\|\\enumerate\\|description\\)")
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (+ LaTeX-item-indent (current-column))))
           (contin (pcase +latex-indent-item-continuation-offset
                     (`auto LaTeX-indent-level)
                     (`align 6)
                     (`nil (- LaTeX-indent-level))
                     (x x))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        LaTeX-item-indent
                        LaTeX-indent-level
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          0))))
                 indent))
            ((looking-at (concat re-end re-env "}"))
             (save-excursion
               (beginning-of-line)
               (ignore-errors
                 (LaTeX-find-matching-begin)
                 (current-column))))
            ((looking-at "\\\\item")
             (+ LaTeX-indent-level indent))
            ((+ contin LaTeX-indent-level indent))))))

(use-package tex
  :straight auctex
  :config
  (setq-default TeX-master t)
  ;; (map-put! TeX-command-list "Check" "chktex -v6 -H %s")
  (jw-setq-hook! TeX-mode-hook
    ispell-parser 'tex)
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-mode-hook #'lsp-deferred 'append)
  ;; (add-hook 'latex-mode-hook #'lsp-deferred)
  (general-def 'normal LaTeX-mode-map
    :prefix "SPC m"
    "v" #'TeX-view
    "c" #'TeX-command-run-all
    "m" #'TeX-command-master)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-to-list 'TeX-view-program-selection '(output-pdf "preview-pane") 'append)
  (add-to-list 'TeX-view-program-list '("preview-pane" latex-preview-pane-mode))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura")))

(use-package latex
  :straight auctex
  :config
  (setq LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label)
        LaTeX-fill-break-at-separators nil
        LaTeX-item-indent 0)
  (dolist (env '("itemize" "enumerate" "description"))
    (add-to-list 'LaTeX-indent-environment-list `(,env +latex-indent-item-fn))))

(use-package preview
  :straight latex-preview-pane
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda ()
                  (* (/ 10.0 (preview-document-pt)) preview-scale)))
  (setq preview-auto-cache-preamble nil)
  (my/leader LaTeX-mode-map
    :infix "m"
    "p" #'preview-at-point
    "P" #'preview-clearout-at-point))

(jw-after! latex-preview-pane
  (setq latex-preview-pane-multifile-mode 'auctex)
  (jw-defadvice! +latex--dont-reopen-preview-pane-a (fn &rest args)
    "Don't reopen the preview pane once it is closed."
    :around #'latex-preview-pane-update
    (cl-letf (((symbol-value 'latex-preview-pane-mode) nil))
      (apply fn args)))
  (keymap-set doc-view-mode-map "q" #'delete-window)
  (keymap-set doc-view-mode-map "ESC" #'delete-window))

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-cite-format
        '((?a . "\\autocite[]{%l}")
          (?b . "\\blockcquote[]{%l}{}")
          (?c . "\\cite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?p . "\\parencite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?t . "\\textcite[]{%l}"))
        reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-fraction 0.3
        ;; This is needed when `reftex-cite-format' is set. See
        ;; https://superuser.com/a/1386206
        LaTeX-reftex-cite-format-auto-activate nil)
  (add-hook 'reftex-mode-hook #'evil-normalize-keymaps)
  (my/leader reftex-mode-map
    :infix "m"
    ";" #'reftex-toc)
  (jw-add-hook! reftex-toc-mode-hook
    (defun +reftex-toc--setup-keymaps-h ()
      (general-def 'local
        "j" #'next-line
        "k" #'previous-line
        "q" #'kill-buffer-and-window
        "ESC" #'kill-buffer-and-window))))

(jw-after! bibtex
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20)
  (keymap-set bibtex-mode-map "C-c \\" #'bibtex-fill-entry))

(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode)
  :hook (org-mode . org-cdlatex-mode)
  :config
  (setq cdlatex-use-dollar-to-ensure-math nil)
  (keymap-unset cdlatex-mode-map "^" t)
  (keymap-unset cdlatex-mode-map "_" t)
  (keymap-unset cdlatex-mode-map "C-RET" t))

(use-package auctex-latexmk
  :after latex
  :straight (auctex-latexmk :host github :repo "jwong101/auctex-latexmk" :branch "auctex-fixup")
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (jw-setq-hook! LaTeX-mode-hook
  	TeX-command-default "LatexMk")
  :config
  (auctex-latexmk-setup))

;;; gdb
(jw-after! gdb
 (setq gdb-show-main t
       gdb-many-windows t))

(use-package perspective
  :after consult
  ;; :custom
  ;; (persp-mode-prefix-key (kbd "SPC x"))
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  :config
  (persp-mode)
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  (setq persp-state-default-file (concat my-emacs-cache-dir "perspective-cache"))
  (keymap-substitute persp-mode-map 'perspective-map nil)
  (general-def 'motion
    :prefix "SPC x"
    "s" #'persp-switch
    "k" #'persp-remove-buffer
    "f" #'persp-forget-buffer
    "c" #'persp-kill
    "r" #'persp-rename
    "a" #'persp-add-buffer
    "A" #'persp-set-buffer
    "b" #'persp-switch-to-buffer
    "B" #'persp-switch-to-scratch-buffer
    "i" #'persp-import
    "n" #'persp-next
    "p" #'persp-prev
    "m" #'persp-merge
    "u" #'persp-unmerge
    "g" #'persp-add-buffer-to-frame-global
    "C-s" #'persp-state-save
    "C-l" #'persp-state-load
    "x" #'persp-switch-by-number))

;; TODO: should probably add this regex to jw-lib
(defun +link-hint--next-uuid (bound)
  "Return the position of the next uuid after the point.
BOUND is the end bound for searching."
  (rx-let ((uuid-chars (num) (= num hex)))
    (funcall #'link-hint--next-regexp
             (rx (or bol blank)
                 (uuid-chars 8)
                 ?- (uuid-chars 4)
                 ?- (uuid-chars 4)
                 ?- (uuid-chars 4)
                 ?- (uuid-chars 12))
             bound)))

(defun +thing-at-point--uuid ()
  "Return if the `thing-at-point' is a uuid."
  (funcall #'thing-at-point 'uuid))

(use-package link-hint
  :defer t
  :init
  (my/leader
    "ho" #'link-hint-open-link
    "hc" #'link-hint-copy-link)
  :config
  (link-hint-define-type 'uuid
    :next #'+link-hint--next-uuid
    :at-point-p #'+thing-at-point--uuid
    :copy #'kill-new)
  (cl-pushnew 'link-hint-uuid link-hint-types))

(use-package woman
  :straight (:type built-in)
  :config
  (evil-add-command-properties #'woman-follow :jump t)
  (general-def 'normal woman-mode-map
    "RET" #'woman-follow))

(defvar inferior-lisp-program "sbcl")

(use-package sly
  :config
  (setq sly-mrepl-history-file-name (expand-file-name "sly-mrepl-history"
                                                      my-emacs-cache-dir)
        sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix)
  (load "~/quicklisp/clhs-use-local.el" 'noerror)
  (sly-setup)
  ;; (remove-hook 'sly-mode-hook #'sly--setup-completion)
  (jw-add-hook +sly--setup-h ()
    ('sly-mode-hook :depth 'append)
    (keymap-set sly--completion-display-mode-map "C-j" #'sly-next-completion)
    (keymap-set sly--completion-display-mode-map "C-k" #'sly-prev-completion)
    (keymap-set sly-minibuffer-map "C-j" #'sly-next-completion)
    (keymap-set sly-minibuffer-map "C-k" #'sly-prev-completion)
    (funcall #'evil-normalize-keymaps)))
    ;; (setq-local completion-at-point-functions
                ;; (list (cape-company-to-capf #'sly-complete-symbol)
                      ;; #'cape-dabbrev
                      ;; #'cape-file))))

(use-package sly-repl-ansi-color
  :after sly
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(use-package sly-macrostep
  :after sly)

(use-package sly-named-readtables
  :after sly)

(use-package sly-quicklisp
  :after sly)

(use-package solaire-mode
  :after doom-themes
  :functions (solaire-global-mode)
  :config
  (solaire-global-mode +1))
