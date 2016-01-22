(when (display-graphic-p)
  (setq frame-title-format '(multiple-frames "%b" "%b"))
  (set-default-font "Inconsolata for Powerline-11")
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
  (toggle-frame-maximized))

(provide 'core-gui)
