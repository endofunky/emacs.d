(use-package xterm-color
  :ensure t
  :custom
  (comint-output-filter-functions
   (remove 'ansi-color-process-output comint-output-filter-functions))
  :config
  (setenv "TERM" "xterm-256color")
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter))

(provide 'pkg-xterm-color)
