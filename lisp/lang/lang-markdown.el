(require 'core-evil)

(use-package markdown-mode
  :mode (("\\.mark$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("\\.mdown$" . markdown-mode)
         ("\\.mdt$" . markdown-mode)
         ("\\.mdwn$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         ("\\.mkdn$" . markdown-mode))
  :defines (markdown-mode-map
            markdown-code-lang-modes)
  :custom
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls nil)
  :config
  (add-to-list 'markdown-code-lang-modes '("go" . go-mode))
  (add-to-list 'markdown-code-lang-modes '("ruby" . ruby-mode))
  (add-to-list 'markdown-code-lang-modes '("rust" . rustic-mode))

  (when window-system
    (define-key markdown-mode-map (kbd "M-RET") 'toggle-frame-fullscreen)))

(use-package evil-markdown
  :after markdown-mode
  :straight (evil-markdown :host github
                           :repo "Somelauw/evil-markdown")
  :hook (markdown-mode . evil-markdown-mode)
  :config
  (add-hook 'evil-markdown-mode-hook #'evil-normalize-keymaps))

(provide 'lang-markdown)
