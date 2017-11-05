(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(provide 'pkg-autorevert)
