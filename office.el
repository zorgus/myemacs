(color-theme-tty-dark)
;(color-theme-comidia)
(setenv "PAGER" "cat")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (list "~/doc/todo.org")))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :family "default"))))
 '(magit-item-highlight ((((class color) (background dark)) (:background "dim gray" :foreground "black"))))
 '(magit-log-head-label ((((class color) (background dark)) (:background "blue")))))
