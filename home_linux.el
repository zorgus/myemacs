;; Eshell environment
;(setenv "PATH" (concat "/opt/codesourcery/arm-2006q3/bin:" (getenv "PATH")))
;(setenv "CROSS_COMPILE" "arm-none-eabi-")
;(setenv "ARCH" "arm")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Grey15" :foreground "Grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "bitstream" :family "Courier 10 Pitch")))))

(color-theme-charcoal-black)
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)  '("NanumGothicCoding" . "unicode-bmp")) ;;; 유니코드 한글영역...Malgun Gothic에다가 원하는폰트를 적는다
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)  '("NanumGothicCoding" . "unicode-bmp")) ;;;유니코드 사용자 영역
(setq x-super-keysym 'meta)

