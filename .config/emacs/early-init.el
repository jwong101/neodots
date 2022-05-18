;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(set-language-environment "UTF-8")

(setq default-input-method nil)

(setq inhibit-compacting-font-caches t)

(setq pgtk-wait-for-event-timeout 0.001)

(setq-default inhibit-redisplay t
	      inhibit-message t)

(add-hook 'window-setup-hook
	  (lambda ()
	    (setq-default inhibit-redisplay nil
			  inhibit-message nil)
	    (redisplay)))

(setq read-process-output-max (* 1024 1024))

(push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(menu-bar-lines . 0) default-frame-alist)
(setq native-comp-async-report-warnings-errors 'silent)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)
(unless after-init-time
  (setq-default mode-line-format nil))
