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
  (markdown-hide-urls t)
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'markdown-mode-map
   "l" '(nil :wk "Links")
   "li" '(markdown-insert-link :wk "Insert")
   "lt" '(markdown-toggle-url-hiding :wk "Toggle")

   "t" '(nil :wk "Table")
   "t=" '(markdown-table-align :wk "Align")
   "tH" '(markdown-table-move-column-left :wk "Move column left")
   "tJ" '(markdown-table-move-row-down :wk "Move row down")
   "tK" '(markdown-table-move-row-up :wk "Move row up")
   "tL" '(markdown-table-move-column-right :wk "Move column right")
   "td" '(markdown-table-delete-column :wk "Delete column")
   "ts" '(markdown-table-sort-lines :wk "Sort rows")
   "tt" '(markdown-insert-table :wk "Create"))
  :config
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
