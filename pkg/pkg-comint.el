(use-package comint
  :defer t
  :config
  (setq comint-scroll-to-bottom-on-output 'others)
  (defun ef-comint-mode-hook ()
    (setq truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil))
  (add-hook 'comint-mode-hook 'ef-comint-mode-hook))

(provide 'pkg-comint)
