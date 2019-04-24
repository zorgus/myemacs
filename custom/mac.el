;;; mac.el --- config for mac                        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  USER

;; Author: USER <user@AL01332675.local>
;; Keywords: lisp, mac

(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)
    )
  ;; meta
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  ;; go
  (setq exec-path (append '("/usr/local/go/bin")
                          exec-path))
  (setq exec-path (append '("/usr/local/bin")
                          exec-path)))

(provide 'mac)
