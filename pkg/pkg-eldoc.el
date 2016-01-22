(use-package eldoc
  :defer t
  :commands eldoc-mode
  :diminish eldoc-mode
  :init
  (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode))

(provide 'pkg-eldoc)
