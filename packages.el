(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy)
  (use-package counsel-projectile
    :ensure t)
  )

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  )

(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  )

(use-package company
  :config
  (global-company-mode t)
  (setq company-dabbrev-downcase nil))

(use-package autoinsert
  :ensure t
  :config
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "template"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode t)
  )

(use-package ag
  :ensure t)

(use-package magit
  :ensure t
  )

(provide 'packages)
