(use-package rfc-mode
  :ensure t
  :commands (rfc-mode-browse)
  :general
  (:states 'normal :prefix ef-prefix
           "hR" '(rfc-mode-browse :wk "Browse RFCs"))
  :config
  (ef-add-popup 'rfc-mode :size 0.5))

(provide 'util-rfc)
