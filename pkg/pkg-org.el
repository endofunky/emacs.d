(use-package org
  :defer t
  :ensure org-plus-contrib
  :pin melpa
  :config
  (require 'org-install)
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-src-tab-acts-natively t
        org-log-done 'time)

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

(provide 'pkg-org)
