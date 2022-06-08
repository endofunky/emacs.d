(use-package markdown-mode
  :straight t
  :mode (("\\.mark$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("\\.mdown$" . markdown-mode)
         ("\\.mdt$" . markdown-mode)
         ("\\.mdwn$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         ("\\.mkdn$" . markdown-mode))
  :custom
  (markdown-hide-urls nil)
  :config
  (defun ef-sp-skip-asterisk (ms mb me)
    "Skip asterisk if at begging of line"
    (save-excursion
      (goto-char mb)
      (save-match-data (looking-at "^\\* "))))

  (sp-local-pair 'markdown-mode "'" nil :actions nil)

  (ef-add-hook markdown-mode-hook
    (sp-with-modes '(markdown-mode)
      (sp-local-pair "`" "`"
                     :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "*" "*"
                     :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :skip-match 'ef-sp-skip-asterisk
                     :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "_" "_")))

  (when window-system
    (define-key markdown-mode-map (kbd "M-RET") 'toggle-frame-fullscreen)))

(provide 'lang-markdown)
