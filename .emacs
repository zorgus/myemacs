;; Hide toolbar
(tool-bar-mode 0)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(current-language-environment "Korean")
)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Andale_Mono")))))
;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; android mode
(load-file "/Developer/Android/android-sdk-mac_x86-1.5_r1/tools/lib/android.el")

;; iswitch mode
(iswitchb-mode t)

;; 한글
;(require 'cl)
;(when enable-multibyte-characters
;  (set-language-environment "Korean")
  
;  (setq-default file-name-coding-system 'utf-8)
  ;; (setq default-korean-keyboard "3")
  ;; (setq input-method-verbose-flag nil
  ;;       input-method-highlight-flag nil)
;  (prefer-coding-system 'utf-8)
;  (set-default-coding-systems 'utf-8)
;  (unless window-system
;    (set-terminal-coding-system 'utf-8)
;    (when (boundp 'encoded-kbd-mode-map)
;      (define-key encoded-kbd-mode-map [27] nil)))
 
;  (set-selection-coding-system 'compound-text-with-extensions)
 
  ;; Hangul Mail setting
;  (setq-default sendmail-coding-system 'euc-kr))
;(unless (or enable-multibyte-characters window-system)
;  (standard-display-european t)
;  (set-input-mode (car (current-input-mode))
;                 (nth 1 (current-input-mode))
;                  0))


;(set-fontset-font "fontset-default" '(#x1100 . #xffdc)  '("AppleGothic" . "unicode-bmp")) ;;; 유니코드 한글영역...Malgun Gothic에다가 원하는폰트를 적는다
;(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)  '("AppleGothic" . "unicode-bmp")) ;;;유니코드 사용자 영역

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

(setenv "PATH" (concat "/opt/android-toolchain/arm-eabi-4.2.1/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
(setenv "CROSS_COMPILE" "arm-eabi-")
(setenv "ARCH" "arm")

;; color theme 설정
(add-to-list 'load-path "~/.emacs.d/utils/color-theme-6.6.0/")
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-blue-sea)

;; twitter mode
(add-to-list 'load-path "~/.emacs.d/")
;(require 'auto-install)
;(setq auto-install-directory "~/.emacs.d/auto-install/")
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

(require 'yasnippet-bundle)
;;short for eshell
(global-set-key (kbd "M-0") 'eshell)
(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)

