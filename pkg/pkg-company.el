(use-package company
  :diminish company-mode
  :ensure t
  :defer 1
  :config
  (defun ts/company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))

  (setq company-auto-complete #'ts/company-visible-and-explicit-action-p
        company-global-modes '(not message-mode help-mode)
        company-begin-commands '(self-insert-command)
        company-minimum-prefix-length 2
        company-idle-delay 0.3
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-margin 2
        company-tooltip-limit 12
        company-require-match 'never
        company-frontends '(company-pseudo-tooltip-frontend))

  (defun ts/check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun ts/tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (ts/check-expansion)
          (company-complete-common)
        (indent-for-tab-command))))

  (defun ts/evil-complete-lambda (arg)
    "Ignores passed in arg like a lambda and runs company-complete"
    (company-complete))

  (setq evil-complete-next-func 'ts/evil-complete-lambda)
  (setq evil-complete-previous-func 'ts/evil-complete-lambda)

  (define-key evil-insert-state-map (kbd "TAB") 'ts/tab-indent-or-complete)
  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-f") 'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map (kbd "TAB")
    'company-select-next-if-tooltip-visible-or-complete-selection)

  (global-company-mode t))

(provide 'pkg-company)
