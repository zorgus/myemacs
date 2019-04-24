;;; config.el --- common config                      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  USER

;; Author: USER <user@AL01332675.local>
;; Keywords: convenience

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

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq column-number-mode t)
(when (version<= "26.0.50" emacs-version )
  (add-hook 'prog-mode-hook #'(lambda () (display-line-numbers-mode t))))
(unless (version<= "26.0.50" emacs-version)
  (add-hook 'prog-mode-hook #'(lambda () (linum-mode t)
                                (setq linum-format "%4d \u2502 ")
                                (set-face-attribute 'linum nil :foreground "gray"))))

(provide 'config)
;;; config.el ends here
