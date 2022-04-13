;;; jw-lib.el --- Summary -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(defun my--delete-all-in-direction! (direction &optional should-kill-buffer)
  "Delete all windows in DIRECTION. If SHOULD-KILL-BUFFER is not nil,
then kill the buffer in that window as well."
  (cl-loop for win = (window-in-direction direction)
           while win
           do (when should-kill-buffer (kill-buffer (window-buffer win)))
           (delete-window win)))

;;;###autoload
(defun jw-delete-vertical-windows! (&optional should-kill-buffer)
  "Delete windows in the same column as this one. If SHOULD-KILL-BUFFER is
not nil, delete the buffers specified in those windows as well."
  (interactive "P")
  (cl-dolist (dir '(up down))
    (my--delete-all-in-direction! dir should-kill-buffer)))

;;;###autoload
(defun jw-delete-horizontal-windows! (&optional should-kill-buffer)
  "Delete windows in the same row as this one. If SHOULD-KILL-BUFFER is
not nil, delete the buffers specified in those windows as well."
  (interactive "P")
  (cl-dolist (dir '(left right))
    (my--delete-all-in-direction! dir should-kill-buffer)))

;;;###autoload
(cl-defmacro jw-add-hook! (hooks body &key funs del depth add local &allow-other-keys)
  "Add FUNS to HOOKS and optionally define a new hook in body."
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let ((func-forms (if (listp funs) funs
                      (list funs)))
        (hook-forms (cl-typecase hooks
                      (consp hooks)
                      (t (list hooks)))))
    (when-let ((fname (cadr body)))
      (push fname func-forms))
    `(progn
       ,body
       (dolist (hook '(,@hook-forms))
         (dolist (func '(,@func-forms))
           ,(if del
                `(remove-hook hook func ,local)
              `(add-hook hook func ,(or depth add) ,local)))))))

;; ;;;###autoload
;; (cl-defmacro jw-defadvice! (symbol args &optional docstring &body body &key places &allow-other-keys)
;;   "Define an advice called SYMBOL and add it to PLACES."
;;   (declare (doc-string 3) (indent defun))
;;   (let ((places-list))
;;     (while (keywordp (car body))
;;       (pop ,body)
;;       (push (pop ,body) places-list))
;;     `(progn
;;        (defun ,symbol ,args ,docstring ,@body)
;;        ((dolist (place (list ,@places-list))
;;           (advice-add place ,where #',symbol))))))

(provide 'jw-lib)
;;; jw-lib.el ends here
