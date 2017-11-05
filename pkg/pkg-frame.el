(use-package frame
  :if window-system
  :init
  (setq frame-title-format '(multiple-frames "%b" "%b")
        frame-resize-pixelwise t
        blink-cursor-blinks 0)

  (set-frame-font "Inconsolata-11")
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
  (add-to-list 'initial-frame-alist '(fullscreen . fullboth)))

(provide 'pkg-frame)
