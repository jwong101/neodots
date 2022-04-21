;;; +eshell.el -*- lexical-binding: t; -*-

;;; Code:
;;;###autoload
(defface +eshell-prompt-pwd '((t (:inherit font-lock-constant-face)))
  "Prompt face."
  :group 'eshell)


;;;###autoload
(defun +eshell-prompt-fn ()
  "Generate the prompt string for eshell.
Use for `eshell-prompt-function'."
  (require 'shrink-path)
  (require 'eshell)
  (declare-function eshell/pwd "eshell")
  (declare-function shrink-path-file "shrink-path")
  (concat (if (bobp) "" "\n")
          (let ((pwd (eshell/pwd)))
            (propertize (if (equal pwd "~")
                            pwd
                          (abbreviate-file-name (shrink-path-file pwd)))
                        'face '+eshell-prompt-pwd))
          (propertize " λ" 'face (if (zerop eshell-last-command-status)
                                     'success
                                   'error))
          " "))

(provide '+eshell)
;;; +eshell.el ends here
