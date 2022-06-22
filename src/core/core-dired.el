(require 'core-lib)

(use-package dired
  :defer t
  :straight nil
  :commands (dired)
  :general
  (:states 'normal :prefix ef-prefix
   "d" '(ef-dired-here :wk "dired Here")
   "D" 'dired)
  (:keymaps 'dired-mode-map
   [remap quit-window] 'kill-this-buffer)
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

(provide 'core-dired)
