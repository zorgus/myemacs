;; My Emacs Setting
;; Author: Munyoung Hwang
;; Email: zorgus@gmail.com

;; file: emacs.el
;; description: common settings for emacs

;; (require 'cl)

;; (global-font-lock-mode 1)               ; syntanx highlight
;; (transient-mark-mode t)                 ; marking highlight
;; (show-paren-mode t)
;; (if (functionp 'global-hi-lock-mode)
;;     (global-hi-lock-mode 1)
;;   (hi-lock-mode 1))
;; ;;(global-hl-line-mode 1)
;; ;(setq ring-bell-function (lambda () nil))

;; (line-number-mode 1)
;; (column-number-mode 1)

;; (setq scroll-step 1)
;; (setq scroll-conservatively 4096)

;; (setq dired-recursive-deletes 'top)	;; dired - recursive delete directory
(setq inhibit-splash-screen t)		;; disable splash screen
                                        
;; (delete-selection-mode 1)

;; ;(setq-default truncate-lines t)

;; (dynamic-completion-mode)

;; ;; Set the text for titlebar and icons, %f=filename, %b=buffername
;; (setq frame-title-format (list "GNU Emacs " emacs-version " - " '(buffer-file-name "%f" "%b")))
;; (setq icon-title-format frame-title-format)

;; (which-function-mode 1)

(tool-bar-mode nil)
(menu-bar-mode nil)
(scroll-bar-mode nil)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; iswitch mode
(iswitchb-mode t)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;; tab, indent
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16))
(setq-default indent-tabs-mode t)
; C mode tab
(add-hook 'c-mode-hook'
(lambda () 
(c-set-style "bsd")
(setq default-tab-width 4)
(setq c-basic-offset 4) ;; indent use only 2 blank
(setq indent-tabs-mode t) ;; no tab
))

;; ;; compilation window 10 height
;; (setq compilation-window-height 10)

;; ;; eshell
;; (global-set-key (kbd "M-0") 'eshell)

;; (setq eshell-save-history-on-exit t)
;; ;(add-hook 'eshell-mode-hook
;; ;          '(lambda () (define-key eshell-mode-map "\t" 'pcomplete-list)))
;; ;(setq eshell-cmpl-cycle-completions nil)

;; ;; org mode
;; (setq org-log-done t)
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; transparency when new frame
(setq transparency-level 80)
(set-frame-parameter nil 'alpha transparency-level)
(add-hook 'after-make-frame-functions (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))
;; transparency frame
(when window-system
(modify-frame-parameters nil '((alpha . 80))))

;; windmove
;; (require 'windmove)
;; (global-set-key (kbd "<M-left>") 'windmove-left)
;; (global-set-key (kbd "<M-right>") 'windmove-down)
;; (global-set-key (kbd "<M-up>") 'windmove-up)
;; (global-set-key (kbd "<M-down>") 'windmove-right)

;; ;; fix problem that open file slowly
(setq vc-handled-backends nil)

;; ;; shortcut for M-x
;; (global-set-key "\C-x\C-m" 'execute-extended-command)
;; (global-set-key "\C-c\C-m" 'execute-extended-command)

;; linum-mode (emacs 23 only)
;; (global-linum-mode 1)
;; (setq linum-format "%5d ")

;; Hangul
;; (when enable-multibyte-characters
;;   (set-language-environment "Korean"))

;(set-fontset-font "fontset-default" '(#x1100 . #xffdc)  '("AppleGothic" . "unicode-bmp"))
;(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)  '("AppleGothic" . "unicode-bmp"))

(when enable-multibyte-characters
 (set-language-environment "Korean")
 (setq-default file-name-coding-system 'utf-8)
 (custom-set-variables '(default-input-method "korean-hangul"))
  ;; (setq default-korean-keyboard "1")
  (setq input-method-verbose-flag nil
        input-method-highlight-flag nil)
 (prefer-coding-system 'utf-8)
 (set-default-coding-systems 'utf-8)
 (unless window-system
   (set-terminal-coding-system 'utf-8)
   (when (boundp 'encoded-kbd-mode-map)
     (define-key encoded-kbd-mode-map [27] nil)))
) 
;  (set-selection-coding-system 'compound-text-with-extensions)
 
  ;; Hangul Mail setting
;  (setq-default sendmail-coding-system 'euc-kr))
;(unless (or enable-multibyte-characters window-system)
;  (standard-display-european t)
;  (set-input-mode (car (current-input-mode))
;                 (nth 1 (current-input-mode))
;                  0))


;;; package settings
;; setting library load path
(add-to-list 'load-path "~/.emacs.d/myemacs/packages/")

;;; magit mode
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Control-tab
;; (global-set-key (kbd "<C-tab>") 'bury-buffer)

;; color theme
(add-to-list 'load-path "~/.emacs.d/myemacs/packages/color-theme-6.6.0/")
(require 'color-theme)
(color-theme-initialize)

;; twitter mode
;; (autoload 'twitter-get-friends-timeline "twitter" nil t)
;; (autoload 'twitter-status-edit "twitter" nil t)
;; (global-set-key "\C-xt" 'twitter-get-friends-timeline)
;; (add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;; yasnippet
;; (require 'yasnippet-bundle)

;; xcscope
(require 'xcscope)
(setq cscope-do-not-update-database t)

;; setnu-mode
;(load-library "setnu")
;(add-hook 'asm-mode-hook 'turn-on-setnu-mode)
;(add-hook 'c-mode-hook 'turn-on-setnu-mode)
;(add-hook 'text-mode-hook 'turn-on-setnu-mode)

;; escreen
;; (require 'escreen)
;; (global-set-key (kbd "C-\\") 'escreen-prefix)

;; cedet
;; (add-to-list 'load-path "~/emacs/packages/cedet/common/")
;; (require 'cedet)
;; (global-ede-mode 1)
;; (semantic-load-enable-code-helpers)
;; (global-srecode-minor-mode 1)

;; ecb
;; (add-to-list 'load-path "~/emacs/packages/ecb-2.40/")
;; (require 'ecb)

;; zenburn
(require 'zenburn)
(zenburn)

;; etags-select
(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)


(defun remove-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(set-fontset-font "fontset-default" '(#x1100 . #xffdc)  '("NanumGothic" . "unicode-bmp")) ;;; 유니코드 한글영역
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)  '("NanumGothic" . "unicode-bmp")) ;;;유니코드 사용자 영역
