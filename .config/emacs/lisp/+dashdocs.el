;;; +dashdocs.el -*- lexical-binding: t; -*-

;;; Code:

(defvar dash-docs-docsets nil)

(defun +dashdocs--consult-search (sync cb)
  (lambda (action)
    (pcase action
      ((pred stringp)
       (when-let (cands (with-current-buffer cb
                          (dash-docs-search action)))
         (funcall sync 'flush)
         (funcall sync cands)))
      (_ (funcall sync action)))))

;;;###autoload
(defun +dashdocs-docsets-lookup (arg &optional query docsets)
  "Lookup QUERY in dash DOCSETS.

If prefix ARG is supplied, search all installed docsets."
  (interactive "P")
  (require 'dash-docs)
  (let ((dash-docs-common-docsets)
        (dash-docs-docsets
         (if arg
             (dash-docs-installed-docsets)
           (cl-remove-if-not #'dash-docs-docsets-path (or docsets dash-docs-docsets)))))
    (dash-docs-initialize-debugging-buffer)
    (dash-docs-create-buffer-connections)
    (dash-docs-create-common-connections)
    (let* ((sink
            (thread-first (consult--async-sink)
                          (consult--async-refresh-immediate)
                          (+dashdocs--consult-search (current-buffer))
                          (consult--async-throttle)))
           (result
            (or (consult--read sink
                               :prompt "Documentation for: "
                               :category 'dash
                               :initial query)
                (user-error "Aborted"))))
      (dash-docs-browse-url (cdr (assoc result (funcall sink nil)))))))

;;;###autoload
(defun +dashdocs-lookup-in-all-docs (&optional query)
  "Lookup QUERY in every docset."
  (interactive)
  (+dashdocs-docsets-lookup t query))

(provide '+dashdocs)
;;; +dashdocs.el ends here
