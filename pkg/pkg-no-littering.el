(use-package no-littering
  :ensure t
  :config
  (when (boundp 'recentf-exclude)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(provide 'pkg-no-littering)
