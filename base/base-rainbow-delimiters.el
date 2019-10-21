(use-package rainbow-delimiters
  :ensure t
  :config
  ;; Only load rainbow-delimiters-mode in prog-mode so it doesn't
  ;; break diffs in magit.
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(provide 'base-rainbow-delimiters)
