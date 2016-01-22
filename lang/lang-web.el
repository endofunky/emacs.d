(use-package web-mode
  :ensure t
  :mode (("\\.html\\+.+\\.liquid$" . web-mode)
         ("\\.html\\.liquid$" . web-mode)
         ("\\.html\\+.+\\.erb$" . web-mode)
         ("\\.html\\.erb$" . web-mode)
         ("\\.html$" . web-mode)
         ("\\.htm$" . web-mode)
         ("\\.html\\+.+$" . web-mode))
  :config
  (setq web-mode-sql-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)

  (defun ts/web-mode-hook ()
    (setq web-mode-enable-auto-pairing nil)
    (sp-local-pair 'web-mode "{ " " }")
    (sp-local-pair 'web-mode "<% " " %>")
    (sp-local-pair 'web-mode "<%= "  " %>")
    (sp-local-pair 'web-mode "<%# "  " %>")
    (sp-local-pair 'web-mode "<%$ "  " %>")
    (sp-local-pair 'web-mode "<%@ "  " %>")
    (sp-local-pair 'web-mode "<%: "  " %>")
    (sp-local-pair 'web-mode "{{"  "}}")
    (sp-local-pair 'web-mode "{{ "  " }}")
    (sp-local-pair 'web-mode "{% "  " %}")
    (sp-local-pair 'web-mode "{%- "  " %}")
    (sp-local-pair 'web-mode "{# "  " #}"))

  (add-hook 'web-mode-hook 'ts/web-mode-hook))

(provide 'lang-web)
