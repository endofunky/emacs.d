(use-package markdown-mode
  :ensure t
  :mode (("\\.mark$" . gfm-mode)
         ("\\.markdown$" . gfm-mode)
         ("\\.md$" . gfm-mode)
         ("\\.mdown$" . gfm-mode)
         ("\\.mdt$" . gfm-mode)
         ("\\.mdwn$" . gfm-mode)
         ("\\.mkd$" . gfm-mode)
         ("\\.mkdn$" . gfm-mode)
         ("\\.text\\'" . gfm-mode)
         ("\\.txt\\'" . gfm-mode))
  :config
  (defun ts/sp-skip-asterisk (ms mb me)
    "Skip asterisk if at begging of line"
    (save-excursion
      (goto-char mb)
      (save-match-data (looking-at "^\\* "))))

  (defun ts/gfm-mode-hook ()
    (sp-with-modes '(gfm-mode)
      (sp-local-pair "`" "`"
                     :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "*" "*"
                     :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :skip-match 'ts/sp-skip-asterisk
                     :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "_" "_")))

  (add-hook 'gfm-mode-hook 'ts/gfm-mode-hook))

(provide 'lang-markdown)
