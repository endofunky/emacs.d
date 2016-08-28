(use-package company
  :ensure t
  :diminish ""
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-echo-delay 0.2)
  (setq company-begin-commands '(self-insert-command))
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 2)

  (setq evil-complete-next-func 'company-select-next-or-abort)
  (setq evil-complete-previous-func 'company-select-previous-or-abort)

  (define-key company-active-map (kbd "TAB") 'company-select-next))

(provide 'pkg-company)

