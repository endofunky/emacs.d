(when (display-graphic-p)
  (setq frame-title-format '(multiple-frames "%b" "%b"))
  (set-default-font "Inconsolata for Powerline-11")
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

  (setq frame-resize-pixelwise t)
  (add-hook 'window-setup-hook 'toggle-frame-maximized))

(provide 'core-gui)
