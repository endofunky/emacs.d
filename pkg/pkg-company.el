(use-package company
  :diminish company-mode
  :ensure t
  :config
  (defun ts/company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))

  (setq company-auto-complete #'ts/company-visible-and-explicit-action-p
        company-global-modes '(not message-mode help-mode)
        company-minimum-prefix-length 2
        company-idle-delay 0.1
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-margin 2
        company-tooltip-limit 12
        company-require-match 'never
        company-frontends '(company-pseudo-tooltip-frontend))

  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-f") 'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map (kbd "TAB")
    'company-select-next-if-tooltip-visible-or-complete-selection)

  (global-company-mode t))

(provide 'pkg-company)
