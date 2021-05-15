(use-package dired
  :defer t
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "q" 'kill-this-buffer)
  :custom
  (dired-dwim-target t))

(use-package diredfl
  :ensure t
  :after dired
  :config
  (diredfl-global-mode))

(provide 'core-dired)
