(setq inhibit-startup-message t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
'("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

(use-package try
:ensure t)

(use-package which-key
:ensure t
:config
(which-key-mode))

(setq myemacs-dir (file-name-directory load-file-name))
(add-to-list 'load-path myemacs-dir)
(defun load!(file)
  (load (concat myemacs-dir file)))

(require 'mac)
(require 'locale)
(require 'config)
(require 'packages)
(require 'c)
