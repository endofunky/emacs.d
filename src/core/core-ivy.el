(require 'core-evil)

(use-package ivy
  :ensure t
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
  :demand t
  :general
  ("M-x" 'counsel-M-x)
  ("M-y" 'counsel-yank-pop)
  (:states 'normal :prefix ef-prefix
	   "/" '(counsel-rg :wk "Grep (rg)")
	   "?" '(counsel-descbinds :wk "Describe Keybinds")
	   "s" '(counsel-switch-buffer :wk "Switch Buffer"))
  :config
  (counsel-mode t))

(use-package swiper
  :after ivy
  :ensure t
  :demand t
  :general
  ("C-s" 'swiper)
  (:states 'normal :prefix ef-prefix
	   "I" '(swiper :wk "Search (swiper)")))

(use-package ivy-xref
  :ensure t
  :after ivy
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'core-ivy)
