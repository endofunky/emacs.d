(use-package company
  :diminish company-mode
  :ensure t
  :config
  (defun ts/company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))

  (setq company-auto-complete #'ts/company-visible-and-explicit-action-p)

  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.05)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-margin 2)
  (setq company-echo-delay 0)
  (setq company-tooltip-limit 12)
  (setq company-require-match 'never)

  (setq company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))

  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map [tab]
    'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "TAB")
    'company-select-next-if-tooltip-visible-or-complete-selection)

  (global-company-mode t))

(provide 'pkg-company)
