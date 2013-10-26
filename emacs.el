;; My Emacs Setting
;; Author: Munyoung Hwang
;; Email: zorgus@gmail.com

;; file: emacs.el
;; description: common settings for emacs

(setq inhibit-splash-screen t)		;; disable splash screen
                                        
(menu-bar-mode -1)

(when window-system
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)  '("NanumGothic" . "unicode-bmp")) ;;; 유니코드 한글영역
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)  '("NanumGothic" . "unicode-bmp")) ;;;유니코드 사용자 영역
;; color theme
(when (= emacs-major-version 23)
  (add-to-list 'load-path "~/.emacs.d/myemacs/packages/color-theme-6.6.0/")
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-xemacs))
(when (= emacs-major-version 24)
	(load-theme 'manoj-dark))
)

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

;; ;; fix problem that open file slowly
(setq vc-handled-backends nil)

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

;;; package settings
;; setting library load path
(add-to-list 'load-path "~/.emacs.d/myemacs/packages/")

;;; magit mode
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)


;; xcscope
(require 'xcscope)
(setq cscope-do-not-update-database t)

;; etags-select
(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)


(defun remove-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

