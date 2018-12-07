(use-package dumb-jump
  :ensure t
  :after evil
  :config
  (defun ef-define-dumb-jump-bindings (keymap)
    "Defines dumb-jump-mode bindings for `KEYMAP'"
    (evil-define-key 'normal keymap ",." 'dumb-jump-back)
    (evil-define-key 'normal keymap ",," 'dumb-jump-go)))

(provide 'pkg-dumb-jump)
