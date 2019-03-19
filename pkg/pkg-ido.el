(use-package ido
  :ensure t
  :custom
  (ido-enable-flex-matching t)
  (ido-create-new-buffer 'always)
  (ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (ido-vertical-show-count t)
  (ido-case-fold t)
  (ido-use-faces t)
  :config
  (evil-define-key 'normal global-map ",s" 'ido-switch-buffer)

  (ido-mode 1)
  (ido-everywhere 1)

  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (add-to-list 'ido-ignore-directories "\\.cache")
  (add-to-list 'ido-ignore-directories "\\.git")
  (add-to-list 'ido-ignore-directories "\\.svn")
  (add-to-list 'ido-ignore-directories "\\.bundle")
  (add-to-list 'ido-ignore-directories "node_modules")
  (add-to-list 'ido-ignore-directories "elpa")
  (add-to-list 'ido-ignore-directories "target"))

(use-package smex
  :after ido
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package ido-completing-read+
  :after ido
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :after ido
  :ensure t
  :config
  (ido-vertical-mode 1))

(use-package flx-ido
  :after ido
  :ensure t
  :custom
  (flx-ido-threshold 2000)
  :config
  (flx-ido-mode 1))

(provide 'pkg-ido)
