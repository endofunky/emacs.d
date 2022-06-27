(require 'core-lib)

(use-package markdown-mode
  :mode (("\\.mark$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("\\.mdown$" . markdown-mode)
         ("\\.mdt$" . markdown-mode)
         ("\\.mdwn$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         ("\\.mkdn$" . markdown-mode))
  :defines (markdown-mode-map)
  :custom
  (markdown-hide-urls nil)
  :config
  (when window-system
    (define-key markdown-mode-map (kbd "M-RET") 'toggle-frame-fullscreen)))

(provide 'lang-markdown)
