;;; locale.el --- config for language                  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  USER

;; Author: USER <user@AL01332675.local>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(when window-system
  (set-fontset-font "fontset-default" '(#x1100 . #xffdc)  '("NanumGothicCoding" . "unicode-bmp"))
  (set-fontset-font "fontset-default" '(#xe0bc . #xf66e) '("NanumGothicCoding" . "unicode-bmp"))
  )


(provide 'locale)
;;; lang.el ends here
