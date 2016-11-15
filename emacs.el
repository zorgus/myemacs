;; My Emacs Setting
;; Author: Munyoung Hwang
;; Email: zorgus@gmail.com

;; file: emacs.el
;; description: common settings for emacs

(setq inhibit-splash-screen t)		;; disable splash screen

;;; package settings
;; setting library load path
(add-to-list 'load-path "~/.emacs.d/myemacs/packages/")
(add-to-list 'load-path "~/.emacs.d/myemacs/auto-install/")

(menu-bar-mode -1)
(when window-system
(tool-bar-mode -1)
(scroll-bar-mode -1)
)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(when window-system
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)  '("NanumGothic" . "unicode-bmp")) ;;; 유니코드 한글영역
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)  '("NanumGothic" . "unicode-bmp")) ;;;유니코드 사용자 영역
;; color theme
(when (= emacs-major-version 23)
  (add-to-list 'load-path "~/.emacs.d/myemacs/packages/color-theme-6.6.0/")
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-xemacs))
(when (= emacs-major-version 24)
	(load-theme 'deeper-blue))
)
(unless window-system
  (set-face-foreground 'minibuffer-prompt "white"))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; (load-file "~/.emacs.d/myemacs/packages/cedet-1.1/common/cedet.el")
;; (global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;; (global-srecode-minor-mode 1)            ; Enable template insertion menu

;; iswitch mode
(iswitchb-mode t)
(which-function-mode)

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

;; fix problem that open file slowly
(setq vc-handled-backends nil)

;; xcscope
(require 'xcscope)
(setq cscope-do-not-update-database t)
(global-set-key "\C-cc" 'cscope-find-global-definition)
(global-set-key "\C-cd" 'cscope-find-this-symbol)
(global-set-key "\C-cp" 'cscope-pop-mark)

;; etags-select
(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

(defun remove-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(require 'find-file-in-repository)
(global-set-key "\C-xf" 'find-file-in-repository)

(defun myplugin ()
  (interactive)
  (defun djcb-opacity-modify (&optional dec)
	"modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
	(let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
		   (oldalpha (if alpha-or-nil alpha-or-nil 100))
		   (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
	  (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
		(modify-frame-parameters nil (list (cons 'alpha newalpha))))))

  ;; C-8 will increase opacity (== decrease transparency)
  ;; C-9 will decrease opacity (== increase transparency
  ;; C-0 will returns the state to normal
  (global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
  (global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
  (global-set-key (kbd "C-0") '(lambda()(interactive)
								 (modify-frame-parameters nil `((alpha . 100)))))

  ;; magit mode
  (require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)


  ;; (semantic-mode)

  (when window-system
	(require 'sr-speedbar)
	(setq sr-speedbar-right-side nil)
	)

  (require 'auto-install)
  (setq auto-install-directory "~/.emacs.d/myemacs/auto-install/")
  ;; (auto-install-update-emacswiki-package-name t)

  (require 'org-install)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)

  (when (>= emacs-major-version 24)
	(require 'package)
	(add-to-list
	 'package-archives
	 '("melpa" . "http://melpa.org/packages/")
	 t)
	(package-initialize))
)
