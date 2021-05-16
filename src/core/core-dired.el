(use-package dired
  :defer t
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "q" 'kill-this-buffer)
  :custom
  (dired-auto-revert-buffer t)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top))

(use-package diredfl
  :ensure t
  :after dired
  :config
  (diredfl-global-mode))

(provide 'core-dired)
