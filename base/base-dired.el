(use-package diredfl
  :ensure t
  :after dired
  :hook (dired-mode . diredfl-mode))

(provide 'base-dired)
