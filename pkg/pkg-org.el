(use-package org
  :defer t
  :ensure org-plus-contrib
  :pin melpa
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-fontify-quote-and-verse-blocks t)
  (org-return-follows-link t)
  (org-hide-emphasis-markers t)
  (org-src-tab-acts-natively t)
  (org-log-done 'time)
  :config
  (require 'org-install)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     (latex . t)
     (ruby . t)))

  (when (display-graphic-p)
    (define-key org-mode-map (kbd "M-RET") 'toggle-frame-fullscreen))

  (evil-define-key 'normal org-mode-map ",t" 'org-todo)
  (evil-define-key 'normal org-mode-map ",c" 'org-toggle-checkbox))

(use-package org-bullets
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-tree-slide
  :after org
  :ensure t
  :custom
  (org-tree-slide-deactivate-message "")
  (org-tree-slide-activate-message "")
  (org-tree-slide-header nil)
  (org-tree-slide-slide-in-effect nil)
  :config
  (require 'ox)
  (require 'ox-latex)

  (ef-add-hook org-tree-slide-play-hook
    (if (fboundp 'flyspell-mode)
        (flyspell-mode -1)
      (flyspell-delete-all-overlays))

    (let ((org-format-latex-options
           (plist-put (copy-tree org-format-latex-options)
		      :scale 4)))
      (org-preview-latex-fragment '(16)))

    (setq-local global-hl-line-mode nil))

  (ef-add-hook org-tree-slide-stop-hook
    (if (fboundp 'flyspell-mode)
        (flyspell-mode t))

    (org-remove-latex-fragment-image-overlays)
    (setq-local global-hl-line-mode nil)
    (setq-local global-hl-line-mode t)))

(use-package demo-it
  :after org
  :ensure t
  :config
  (evil-define-key 'normal demo-it-mode-map (kbd "<SPC>") 'demo-it-step)
  (evil-define-key 'normal demo-it-mode-map (kbd "<right>") 'demo-it-step)
  (evil-define-key 'normal demo-it-mode-map (kbd "<left>") 'demo-it-restep)
  (evil-define-key 'normal demo-it-mode-map (kbd "q") 'demo-it-end)
  (evil-define-key 'normal demo-it-mode-map (kbd "<tab>") 'show-all))

(use-package expand-region
  :after demo-it
  :ensure t)

(use-package fancy-narrow
  :after demo-it
  :ensure t)

(provide 'pkg-org)
