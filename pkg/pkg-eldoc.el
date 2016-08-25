(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :init
  (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode))

(provide 'pkg-eldoc)
