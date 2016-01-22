(use-package rainbow-delimiters
  :ensure t
  :config
  (define-globalized-minor-mode global-rainbow-delimiters-mode
    rainbow-delimiters-mode rainbow-delimiters-mode)
  (global-rainbow-delimiters-mode t))

(provide 'pkg-rainbow-delimiters)
