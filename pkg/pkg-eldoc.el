(use-package eldoc
  :diminish eldoc-mode
  :config
  (setq eldoc-idle-delay 0.5)

  ;; Eldoc massively slows down cursor movement. This advice fixes that.
  (advice-add 'eldoc-pre-command-refresh-echo-area :override #'ignore))

(provide 'pkg-eldoc)
