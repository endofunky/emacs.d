(use-package web-mode
  :ensure t
  :mode (("\\.html\\+.+\\.liquid$" . web-mode)
         ("\\.html\\.liquid$" . web-mode)
         ("\\.html\\+.+\\.erb$" . web-mode)
         ("\\.html\\.erb$" . web-mode)
         ("\\.html$" . web-mode)
         ("\\.htm$" . web-mode)
         ("\\.html\\+.+$" . web-mode))
  :custom
  (web-mode-sql-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight nil)
  (web-mode-enable-auto-pairing nil)
  :config

  (defun ef-web-mode-hook ()
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

  (add-hook 'web-mode-hook 'ef-web-mode-hook))

(provide 'lang-web)
