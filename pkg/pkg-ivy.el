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
  (if (executable-find "rg")
      (evil-define-key 'normal global-map ",/" 'counsel-rg))
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (evil-define-key 'normal global-map ",?" 'counsel-descbinds)
  (evil-define-key 'normal global-map ",s" 'counsel-switch-buffer))

(use-package swiper
  :after ivy
  :ensure t
  :config
  (global-set-key (kbd "C-s" ) 'swiper)
  (evil-define-key 'normal global-map ",I" 'swiper))

(use-package ivy-xref
  :ensure t
  :after ivy
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'pkg-ivy)
