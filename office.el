(color-theme-comidia)
(setq dired-recursive-deletes 'top)	;; dired - recursive delete directory
(setenv "PAGER" "cat")
(setq inhibit-splash-screen t)		;; disable splash screen

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (list "~/doc/todo.org"))
 '(ecb-options-version "2.32")
 '(ecb-source-path (quote (("/home/munyoung/sources/adam-sys/LINUX/kernel" "kernel") ("/home/munyoung/sources/adam-sys/LINUX/android" "android"))))
 '(standard-indent 8))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :family "default")))))
