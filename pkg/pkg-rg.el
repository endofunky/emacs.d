(use-package deadgrep
  :ensure t
  :commands deadgrep
  :init
  (define-key evil-normal-state-map ",/" #'deadgrep)
  :config
  (setq deadgrep-project-root-function #'projectile-project-root)

  (defadvice deadgrep (before my activate)
    (xref-push-marker-stack))

  (defadvice deadgrep-visit-result (before my activate)
    (xref-push-marker-stack))

  (evil-define-key 'normal deadgrep-mode-map ",." #'pop-tag-mark))

(provide 'pkg-rg)