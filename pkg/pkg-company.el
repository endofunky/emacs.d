(use-package company
  :ensure t
  :diminish ""
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
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
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  ;; Set some more readable colors for the completion popup.
  (set-face-attribute 'company-tooltip nil :background "#0d0f01" :foreground "white")
  (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "#81a2be" :foreground "#1d1f21")
  (set-face-attribute 'company-preview nil :background "#0d0f01")
  (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "#c5c8c6")
  (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip :foreground "#b5bd68")
  (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection :foreground "white")
  (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
  (set-face-attribute 'company-scrollbar-fg nil :background "gray40"))

(provide 'pkg-company)

