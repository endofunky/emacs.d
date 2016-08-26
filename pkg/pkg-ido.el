(use-package ido
  :ensure t
  :config
  (evil-define-key 'normal global-map ",s" 'ido-switch-buffer)

  (ido-mode 1)
  (ido-everywhere 1)

  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (setq ido-vertical-show-count t)
  (setq ido-case-fold t)
  (setq ido-use-faces t)

  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (add-to-list 'ido-ignore-directories "\\.cache")
  (add-to-list 'ido-ignore-directories "\\.git")
  (add-to-list 'ido-ignore-directories "\\.svn")
  (add-to-list 'ido-ignore-directories "\\.bundle")
  (add-to-list 'ido-ignore-directories "node_modules")
  (add-to-list 'ido-ignore-directories "elpa")
  (add-to-list 'ido-ignore-directories "target")

  (use-package smex
    :ensure t
    :bind (("M-x" . smex)
           ("M-X" . smex-major-mode-commands))
    :config
    (smex-initialize))

  (use-package ido-ubiquitous
    :ensure t
    :config
    (ido-ubiquitous-mode 1))

  (use-package ido-vertical-mode
    :ensure t
    :config
    (ido-vertical-mode 1))

  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode 1)
    (setq flx-ido-threshold 2000)))

(provide 'pkg-ido)
