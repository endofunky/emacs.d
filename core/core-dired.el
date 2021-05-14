(use-package dired
  :defer t
  :custom
  (dired-dwim-target t))

(use-package diredfl
  :ensure t
  :after dired
  :config
  (diredfl-global-mode))

(provide 'core-dired)
