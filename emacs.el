;;; .emacs ���� ���� ����� ������Ʈ
;; 1. ������ emacs �ɼ��� ã�ƶ�!!
;; 2. �� mode�� ����
;; 3. ������ plugin�� ��Ű��ȭ ����
;; 4. �޸��� ��Ȱȭ
;; 5. elisp�� �ͼ��� ����
;; 6. ���ϴ� ������ �˰� ����

(require 'cl)


;;; ���� ȯ�濡�� ���� ���� ��������

(defconst win32p  (eq system-type 'windows-nt) "�����ӽ��̸� ��")
(defconst unixp   (eq system-type (or 'gnu/linux 'berkeley-unix)) "FreeBSD �ӽ��̸� ��")
(defconst homep   (string-match "MOONFIRE" system-name)"���� pc ��� ��")
(defconst officep (not homep)"�繫���� pc ��� ��")
(defconst extra-packages "~/.emacs.d" "���� �߰��� ��ġ�� el ��Ű������ ��ġ")

;environment setting
(setenv "PATH" (concat "/opt/android-toolchain/arm-eabi-4.2.1/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
(setenv "CROSS_COMPILE" "arm-eabi-")
(setenv "ARCH" "arm")


(global-font-lock-mode 1)               ; syntanx highlight
(transient-mark-mode t)                 ; marking highlight
(show-paren-mode t)                     ; ¦�� �´� ��ȣ �����ش�
(if (functionp 'global-hi-lock-mode) ; C-x w h ������ Ư�� �ܾ���� �����ش�
    (global-hi-lock-mode 1)
  (hi-lock-mode 1))
;;(global-hl-line-mode 1)                 ; �������� �����ش�. �̰� �� �����ؼ� ����.
(setq ring-bell-function (lambda () nil)) ; bell ����

(line-number-mode 1)                  ; mode line �� ���μ��� ǥ���Ѵ�
(column-number-mode 1) ; mode line �� �÷��� ǥ���Ѵ�(�⺻�� �ƴϴ���)

(setq scroll-step 1)                    ; �������� ��ũ���� ���ؼ�..
(setq scroll-conservatively 4096)

                                        
(delete-selection-mode 1)		; ������ó��, ���õ� regeion �� DEL �� ����ų�, �ٸ� ���ڸ� Ÿ���� �Ҷ� ��� �����.

(setq-default truncate-lines t) ; ȭ���� ����� �� ��ó�� toggle-truncate-lines ����

;;(dynamic-completion-mode)               ; �� �̰� �����? M-/ ���� M-RET ����

;; Set the text for titlebar and icons, %f=filename, %b=buffername
(setq frame-title-format (list "GNU Emacs " emacs-version " - " '(buffer-file-name "%f" "%b")))
(setq icon-title-format frame-title-format)

(which-function-mode 1)	   ; �Լ� ǥ��

(tool-bar-mode -1)	   ; ���� �Ⱦ��ϱ� ����

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; ��ũ�ѹ� ���� �Ⱦ���.

;; iswitch mode
(iswitchb-mode t)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;;short for eshell
(global-set-key (kbd "M-0") 'eshell)

;; �ѱ�
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


;(set-fontset-font "fontset-default" '(#x1100 . #xffdc)  '("AppleGothic" . "unicode-bmp")) ;;; �����ڵ� �ѱۿ���...Malgun Gothic���ٰ� ���ϴ���Ʈ�� ���´�
;(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)  '("AppleGothic" . "unicode-bmp")) ;;;�����ڵ� ����� ����


;;; ������ �÷������� �������

;; setting library load path
(add-to-list 'load-path "~/.emacs.d/")

;;; magit mode
(require 'magit)

;; Control-tab���� ���� �̵��ϱ�
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; color theme ����
(add-to-list 'load-path "~/.emacs.d/utils/color-theme-6.6.0/")
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-blue-sea)

;; twitter mode
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;; yasnippet
;(require 'yasnippet-bundle)

;setting for MAC
(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)
