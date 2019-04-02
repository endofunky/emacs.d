(use-package deadgrep
  :ensure t
  :commands deadgrep
  :init
  (define-key evil-normal-state-map ",/" #'deadgrep)
  :config
  (setq deadgrep-project-root-function #'projectile-project-root))

(provide 'pkg-rg)
