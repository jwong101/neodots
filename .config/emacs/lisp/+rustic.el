;;; +rustic.el --- Helpers for rust mode. -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'cl-lib)

(defun +rustic-cargo-asm-search ()
  "TODO.")

(cl-defun +rustic-cargo-asm-test (&rest args)
  "TODO: Call cargo-asm."
  (interactive)
  (let* ((buffer (get-buffer-create +cargo-asm-buffer+))
         (prompt
          (with-current-buffer buffer
            (setq-local default-directory "~/test-workspace/rust/rust-book/")
            (erase-buffer)
            (apply #'call-process "cargo-asm" nil (cons buffer nil) nil args)
            (goto-char (point-min))
            (cl-flet ((trim ()
                        (cl-incf (point))
                        (delete-region (point-min) (point))))
              (cond ((re-search-forward (rx bow "Finished" (* any) "target" (* any) "s" eol) nil t)
                     (trim) nil)
                    ((re-search-forward (rx bol "Try one of those" eol) nil t)
                     (trim) (buffer-substring-no-properties (point-min) (point-max)))
                    (t (cl-return)))))))
    (if prompt (call-interactively #'+rustic-cargo-asm-search)
      (display-buffer buffer t))))

;; (+rustic-cargo-asm-test "core")
(provide '+rustic)
;;; +rustic.el ends here
