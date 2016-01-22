(use-package company
  :ensure t
  :defer 1
  :diminish ""
  :config
  (global-company-mode)
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 1)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 2)

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

  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(provide 'pkg-company)

