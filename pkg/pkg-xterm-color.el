(use-package xterm-color
  :ensure t
  :config
  (setenv "TERM" "xterm-256color")
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))

(provide 'pkg-xterm-color)
