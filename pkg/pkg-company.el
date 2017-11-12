(use-package company
  :diminish company-mode
  :ensure t
  :config
  (defun ef-company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))

  (setq company-auto-complete #'ef-company-visible-and-explicit-action-p
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

  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-f") 'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map [tab]
    'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "TAB")
    'company-select-next-if-tooltip-visible-or-complete-selection)

  (global-company-mode t))

(use-package company-statistics
  :ensure t
  :after company
  :config
  (company-statistics-mode))

(use-package evil
  :after company
  :defer t
  :config
  (defun ef-check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun ef-tab-indent-or-complete ()
    "Either indent or start company completion."
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (ef-check-expansion)
          (company-complete-common)
        (indent-for-tab-command))))

  (defun ef-evil-complete-lambda (arg)
    "Ignores passed in arg like a lambda and runs company-complete"
    (company-complete))

  (setq evil-complete-next-func 'ef-evil-complete-lambda)
  (setq evil-complete-previous-func 'ef-evil-complete-lambda)

  (define-key evil-insert-state-map (kbd "TAB") 'ef-tab-indent-or-complete)

  (define-key minibuffer-local-map [tab] 'company-complete)
  (define-key minibuffer-local-map (kbd "TAB") 'company-complete)
  (define-key evil-ex-completion-map [tab] 'company-complete)
  (define-key evil-ex-completion-map (kbd "TAB") 'company-complete)
  (define-key evil-ex-completion-map [remap completion-at-point] 'company-complete)

  (defun ef-minibuffer-completion-hook ()
    (setq-local company-frontends '(company-preview-frontend))
    (company-mode t))

  (add-hook 'minibuffer-setup-hook 'ef-minibuffer-completion-hook))

(provide 'pkg-company)
