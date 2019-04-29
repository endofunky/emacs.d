(use-package which-key
  :defer 5
  :diminish
  :commands which-key-mode
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0)
  :config
  (which-key-mode))

(provide 'pkg-which-key)
