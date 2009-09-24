;;; .emacs 설정 파일 만들기 프로젝트
;; 1. 유용한 emacs 옵션을 찾아라!!
;; 2. 각 mode별 설정
;; 3. 유용한 plugin을 패키지화 하자
;; 4. 메모의 생활화
;; 5. elisp에 익숙해 지자
;; 6. 머하는 놈인지 알고 쓰자

(require 'cl)


;;; 여러 환경에서 쓰기 위한 설정값들

;(defconst win32p  (eq system-type 'windows-nt) "윈도머신이면 참")
;(defconst unixp   (eq system-type (or 'gnu/linux 'berkeley-unix)) "FreeBSD 머신이면 참")
;(defconst homep   (string-match "MOONFIRE" system-name)"집의 pc 라면 참")
;(defconst officep (not homep)"사무실의 pc 라면 참")
;(defconst extra-packages "~/.emacs.d" "내가 추가로 설치한 el 패키지들의 위치")

(global-font-lock-mode 1)               ; syntanx highlight
(transient-mark-mode t)                 ; marking highlight
(show-paren-mode t)                     ; 짝이 맞는 괄호 보여준다
(if (functionp 'global-hi-lock-mode) ; C-x w h 등으로 특정 단어들을 빛내준다
    (global-hi-lock-mode 1)
  (hi-lock-mode 1))
;;(global-hl-line-mode 1)                 ; 현재줄을 빛내준다. 이거 좀 불편해서 뺐다.
;(setq ring-bell-function (lambda () nil)) ; bell 무시

(line-number-mode 1)                  ; mode line 에 라인수를 표시한다
(column-number-mode 1) ; mode line 에 컬럼을 표시한다(기본이 아니더라)

(setq scroll-step 1)                    ; 윈도스런 스크롤을 위해서..
(setq scroll-conservatively 4096)

(setq dired-recursive-deletes 'top)	;; dired - recursive delete directory
(setq inhibit-splash-screen t)		;; disable splash screen
                                        
(delete-selection-mode 1)		; 윈도우처럼, 선택된 regeion 을 DEL 로 지우거나, 다른 글자를 타이핑 할때 즉시 지운다.

(setq-default truncate-lines t) ; 화면을 벗어나는 긴 줄처리 toggle-truncate-lines 참고

(dynamic-completion-mode)               ; 음 이게 뭐드라? M-/ 던가 M-RET 던가

;; Set the text for titlebar and icons, %f=filename, %b=buffername
(setq frame-title-format (list "GNU Emacs " emacs-version " - " '(buffer-file-name "%f" "%b")))
(setq icon-title-format frame-title-format)

(which-function-mode 1)	   ; 함수 표시

(tool-bar-mode -1)	   ; 툴바 안쓰니까 제거
(menu-bar-mode -1)	   ; 메뉴바도 제거

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; 스크롤바 거의 안쓴다.

;; iswitch mode
(iswitchb-mode t)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;; tab, indent
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16))
(setq-default indent-tabs-mode nil)
; C mode tab
(add-hook 'c-mode-hook'
(lambda () 
(c-set-style "bsd")
(setq default-tab-width 4)
(setq c-basic-offset 4) ;; indent use only 2 blank
(setq indent-tabs-mode nil) ;; no tab
))

;; compilation window는 항상 10 height크기로 뜨도록
(setq compilation-window-height 10)

;; for eshell
(global-set-key (kbd "M-0") 'eshell)
; 종료 시에 물어보지 말고 저장하고 종료
(setq eshell-save-history-on-exit t)
;(add-hook 'eshell-mode-hook
;          '(lambda () (define-key eshell-mode-map "\t" 'pcomplete-list)))
;(setq eshell-cmpl-cycle-completions nil)


;; 한글
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


;;; 유용한 플러그인을 사용하자

;; setting library load path
(add-to-list 'load-path "~/emacs/packages/")

;;; magit mode
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Control-tab으로 버퍼 이동하기
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; color theme 설정
(add-to-list 'load-path "~/emacs/packages/color-theme-6.6.0/")
(require 'color-theme)
(color-theme-initialize)
;(color-theme-blue-sea)

;; twitter mode
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;; yasnippet
(require 'yasnippet-bundle)

;; xcscope
(require 'xcscope)
(setq cscope-do-not-update-database t)

;setting for MAC
(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)

