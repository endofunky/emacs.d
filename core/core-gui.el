(when (display-graphic-p)
  (setq frame-title-format '(multiple-frames "%b" "%b"))
  (set-frame-font "Inconsolata for Powerline-11")
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

  (setq frame-resize-pixelwise t)
  (add-hook 'window-setup-hook 'toggle-frame-maximized)

  (set-fringe-mode 0))

(provide 'core-gui)
