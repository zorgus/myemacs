(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(current-language-environment "Korean")
 '(magit-git-executable "/opt/local/bin/git")
)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Andale_Mono")))))

;; android mode
(load-file "/Developer/Android/android-sdk-mac_x86-1.5_r1/tools/lib/android.el")

;; color-theme
(color-theme-late-night)

;; setting for MAC
(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)

