;;; jw-lib.el --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Gotta add jw as a prefix to everything in case I get screwed over by
;;; elisp's lack of namespacing.

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(defun my--delete-all-in-direction! (direction &optional should-kill-buffer)
  "Delete all windows in DIRECTION.
If SHOULD-KILL-BUFFER is not nil,then kill the buffer in that window as well."
  (cl-loop for win = (window-in-direction direction)
           while win
           do (when should-kill-buffer (kill-buffer (window-buffer win)))
           (delete-window win)))

(defmacro jw-run-with-interval (interval idle &rest rest)
  "Run REST every INTERVAL seconds, unless idle for IDLE seconds."
  (declare (indent 2))
  (let ((idle-time (gensym "idle-time"))
        (idle-secs (gensym "idle-secs")))
    `(progn
       (run-at-time nil ,interval
                    (lambda ()
                      (when-let* ((,idle-time (current-idle-time))
                                  (,idle-secs (float-time ,idle-time)))
                        (unless (> ,idle-secs ,idle)
                          ,@rest))))
       (run-with-idle-timer ,idle t (lambda () ,@rest)))))

(defmacro jw-shutup (&rest rest)
  "Run REST silently."
  (declare (indent 0) (debug t))
  `(let ((inhibit-message t)
         (save-silently t))
     (cl-letf (((symbol-function 'message) #'ignore))
       ,@rest)))

(defun jw-shutup-a (old &rest args)
  "Advice wrapper.
Call OLD with ARGS."
  (jw-shutup (apply old args)))

;;;###autoload
(defun jw-delete-vertical-windows! (&optional should-kill-buffer)
  "Delete windows in the same column as this one.
If SHOULD-KILL-BUFFER is not nil, delete the buffers specified in those windows
as well."
  (interactive "P")
  (cl-dolist (dir '(up down))
    (my--delete-all-in-direction! dir should-kill-buffer)))

;;;###autoload
(defun jw-delete-horizontal-windows! (&optional should-kill-buffer)
  "Delete windows in the same row as this one.
If SHOULD-KILL-BUFFER is not nil, delete the buffers specified in those
windows as well."
  (interactive "P")
  (cl-dolist (dir '(left right))
    (my--delete-all-in-direction! dir should-kill-buffer)))

;;;###autoload
(cl-defmacro jw-add-hook! (hooks body &key funs del depth add local &allow-other-keys)
  "Add FUNS to HOOKS and optionally define a new hook in BODY."
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

(defun jw-tolist (form)
  "Return FORM wrapped in a new list, unless it is already a list."
  (declare (pure t) (side-effect-free t))
  (if (proper-list-p form) form (list form)))

(defmacro jw-remap! (keymap &rest rest)
  "Remap OLDDEF with NEWDEF in KEYMAP.
Basically a variadic version of `keymap-substitute'.

\(fn KEYMAP [OLDDEF NEWDEF]... &rest REST\)"
  (declare (indent 1))
  (let ((cells (make-symbol "cells")))
    `(cl-loop for ,cells on (list ,@rest) by #'cddr
              do (keymap-substitute ,keymap (car ,cells) (cadr ,cells)))))

(defmacro jw-global-remap! (&rest rest)
  "Remap OLDDEF with NEWDEF in `global-map'.
REST is of the form [OLDDEF NEWDEF]..."
  (declare (indent 1))
  (macroexpand `(jw-remap! global-map ,@rest)))

(cl-defmacro jw-pushnew! (place &rest rest &key (test #'equal) &allow-other-keys)
  "Push REST into PLACE if they aren't already present.
Like `cl-pushnew', but variadic."
  (declare (indent 1))
  (while (keywordp (car rest))
    (setq rest (cddr rest)))
  (let ((b (make-symbol "b")))
    `(dolist (,b (list ,@rest))
       (cl-pushnew ,b ,place :test #',test))))

(defmacro jw-defadvice! (symbol args &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGS, DOCSTRING, and BODY are like components in `defun'.
From Doom's defadvice! macro.

\(fn SYMBOL ARGS &optional DOCSTRING [WHERE PLACES...] &body BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let ((places-alist)
        (places (gensym))
        (place (gensym)))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (jw-tolist ,(pop body))) places-alist))
    `(progn
       (defun ,symbol ,args ,docstring ,@body)
       (dolist (,places (list ,@places-alist))
          (dolist (,place (cdr ,places))
            (advice-add ,place (car ,places) #',symbol))))))

(defmacro jw-after! (package &rest body)
  "Eval BODY after PACKAGE has loaded.
Basically like `eval-after-load', except prevents eager expansion pulling in
autoloads."
  (declare (indent defun) (debug t))
  (when (symbolp package)
    (list (if (or (not (bound-and-true-p byte-compile-current-file))
                  (require package nil 'noerror))
              #'progn
            #'with-no-warnings)
          `(eval-after-load ',package ',(macroexp-progn body)))))

(provide 'jw-lib)
;;; jw-lib.el ends here
