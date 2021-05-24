(use-package dired
  :defer t
  :commands (dired)
  :general
  (:states 'normal :prefix ef-prefix
   "d" 'dired)
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
  :defer t
  :hook
  (dired-mode . diredfl-mode))

(provide 'core-dired)
