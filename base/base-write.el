(use-package hide-mode-line
  :ensure t)

(use-package writegood-mode
  :ensure t)

(define-derived-mode write-mode
  text-mode "Write"
  "Major mode for writing."
  (flyspell-mode t)
  (writegood-mode t)
  (visual-line-mode t))

(provide 'base-write)