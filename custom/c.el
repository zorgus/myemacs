;;; c.el --- c language config                       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  USER

;; Author: USER <user@AL01332675.local>
;; Keywords: c

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

(use-package irony
  :ensure t
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode))
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (use-package company-irony-c-headers
    :ensure t
    )

  (use-package company-irony
    :ensure t
    :hook ((irony-mode . company-mode))
    :config
    (add-to-list (make-local-variable 'company-backends) '(company-irony company-irony-c-headers))
    )

  (use-package flycheck-irony
    :ensure t
    :config
    (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
    )

  (use-package irony-eldoc
    :ensure t
    :config
    (add-hook 'irony-mode-hook #'irony-eldoc)
    )
  )

(use-package flycheck
  :ensure t
  :hook ((c-mode-hook . flycheck-mode))
  )

(use-package rtags
  :ensure t
  :config
  (rtags-enable-standard-keybindings)
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  ;; (rtags-start-process-unless-running)
  (use-package ivy-rtags
    :ensure t
    )
  (setq rtags-display-result-backend 'ivy)
  )

(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup)
  (setq
   cmake-ide-build-dir "build"
   cmake-ide-cmake-opts "-DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
  )

(provide 'c)
;;; c.el ends here
