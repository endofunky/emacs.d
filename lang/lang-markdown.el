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
         ("\\.txt\\'" . gfm-mode)))

(provide 'lang-markdown)
