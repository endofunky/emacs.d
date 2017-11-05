(use-package whitespace
  :config
  (global-whitespace-mode 1)
  (setq-default show-trailing-whitespace nil)
  (setq whitespace-style (quote (face trailing)))
  :diminish (whitespace-mode global-whitespace-mode))

(provide 'pkg-whitespace)
