(require 'core-evil)
(require 'core-shackle)

(use-package ascii-table
  :ensure t
  :general
  (:states 'normal :prefix ef-prefix
   "hA" '(ascii-table :wk "ASCII Table"))
  :config
  (add-to-list 'evil-emacs-state-modes 'ascii-table-mode)
  (ef-add-popup "*ASCII*" :ephemeral t :size 20))

(provide 'util-ascii-table)
