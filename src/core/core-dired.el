(use-package dired
  :defer t
  :commands (dired)
  :general
  (:states 'normal :prefix ef-prefix
   "d" '(ef-dired-here :wk "dired Here")
   "D" 'dired)
  (:states 'normal :keymaps 'dired-mode-map
   "q" 'kill-this-buffer)
  (:states 'normal :prefix ef-prefix :keymaps 'dired-mode-map
   "d" 'bury-buffer
   "D" 'bury-buffer)
  :init
  (defun ef-dired-here ()
    "Open dired buffer in current directory."
    (interactive)
    (dired "."))
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
