(use-package markdown-mode
  :ensure t
  :mode (("\\.mark$" . gfm-mode)
         ("\\.markdown$" . gfm-mode)
         ("\\.md$" . gfm-mode)
         ("\\.mdown$" . gfm-mode)
         ("\\.mdt$" . gfm-mode)
         ("\\.mdwn$" . gfm-mode)
         ("\\.mkd$" . gfm-mode)
         ("\\.mkdn$" . gfm-mode))
  :custom
  (markdown-hide-urls nil)
  :config
  (defun ef-sp-skip-asterisk (ms mb me)
    "Skip asterisk if at begging of line"
    (save-excursion
      (goto-char mb)
      (save-match-data (looking-at "^\\* "))))

  (sp-local-pair 'gfm-mode "'" nil :actions nil)
  (sp-local-pair 'markdown-mode "'" nil :actions nil)

  (defun ef-gfm-mode-hook ()
    (turn-on-orgtbl)
    (sp-with-modes '(gfm-mode)
      (sp-local-pair "`" "`"
                     :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "*" "*"
                     :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :skip-match 'ef-sp-skip-asterisk
                     :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "_" "_")))

  (add-hook 'gfm-mode-hook 'ef-gfm-mode-hook)

  (when (display-graphic-p)
    (define-key markdown-mode-map (kbd "M-RET") 'toggle-frame-fullscreen)))

(provide 'lang-markdown)
