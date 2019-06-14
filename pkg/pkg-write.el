(use-package hide-mode-line
  :ensure t)

(use-package writegood-mode
  :ensure t)

(define-derived-mode write-mode
  text-mode "Write"
  "Major mode for writing."
  (hide-mode-line-mode t)
  (flyspell-mode t)
  (writegood-mode t)
  (visual-line-mode t))

(provide 'pkg-write)
