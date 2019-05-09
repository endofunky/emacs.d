(use-package ivy
  :ensure t
  :diminish ivy-mode
  :custom
  (ivy-fixed-height-minibuffer t)
  (ivy-format-function #'ivy-format-function-line)
  (ivy-height 12)
  (ivy-magic-slash-non-match-action nil)
  (ivy-wrap t)
  :config
  (ivy-mode t)
  (setq ivy-flx-limit 2000)
  (setq ivy-re-builders-alist '((counsel-ag . ivy--regex-plus)
                                (counsel-rg . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))

(use-package flx
  :after ivy
  :ensure t)

(use-package smex
  :after ivy
  :ensure t)

(use-package counsel
  :after ivy
  :ensure t
  :diminish counsel-mode
  :config
  (counsel-mode t)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (if (executable-find "ripgrep")
      (evil-define-key 'normal global-map ",/" 'counsel-rg))
  (evil-define-key 'normal global-map ",?" 'counsel-descbinds)
  (evil-define-key 'normal global-map ",s" 'counsel-switch-buffer))

(provide 'pkg-ivy)
