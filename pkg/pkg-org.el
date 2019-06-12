(require 'pkg-shackle)

(use-package org
  :defer t
  :ensure org-plus-contrib
  :pin melpa
  :mode (("\\.\\(org\\|org_archive\\)$" . org-mode))
  :custom
  (org-agenda-custom-commands
   '(("n" "Agenda and all TODOs" ((agenda "") (alltodo ""))
      ((org-agenda-span 'week)
       (org-deadline-warning-days 0)
       (org-agenda-skip-deadline-prewarning-if-scheduled t)
       (org-agenda-skip-deadline-if-done t)
       (org-agenda-skip-scheduled-if-deadline-is-shown t)
       (org-agenda-todo-ignore-deadlines 'all)
       (org-agenda-todo-ignore-scheduled 'all)))))
  (org-agenda-files `(,(expand-file-name "~/Dropbox/org/")))
  (org-agenda-restore-windows-after-quit nil)
  (org-agenda-start-on-weekday 1)
  (org-agenda-time-grid
   '((daily today remove-match require-timed)
     (600 800 1000 1200 1400 1600 1800 2000 2200)
     "......" "----------------"))
  (org-agenda-window-setup 'other-window)
  (org-archive-location "%s_archive::datetree/* Archived Tasks")
  (org-confirm-babel-evaluate nil)
  (org-deadline-warning-days 7)
  (org-default-notes-file (expand-file-name "~/Dropbox/org/notes.org"))
  (org-enforce-todo-dependencies t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-log-done 'time)
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  :commands (org-agenda org-capture ef-org-agenda)
  :hook
  (org-mode . flyspell-mode)
  :init
  (defun ef-org-notes ()
    (interactive)
    (find-file org-default-notes-file))

  (evil-define-key 'normal global-map ",O" 'ef-org-agenda)
  (evil-define-key 'normal global-map ",oa" 'org-agenda)
  (evil-define-key 'normal global-map ",oc" 'org-capture)
  (evil-define-key 'normal global-map ",oo" 'ef-org-notes)
  :config
  (require 'org-capture)
  (require 'org-install)

  (setq org-capture-templates
        (append org-capture-templates
                '(("t" "Todo" entry (file+headline "~/Dropbox/org/notes.org" "Tasks")
                   "** TODO %^{Task} %?"))))

  (defun org-switch-to-buffer-other-window (&rest args)
    (apply 'switch-to-buffer-other-window args))

  (ef-keep-other-windows org-agenda)

  (defun ef-org-agenda ()
    (interactive)
    (org-agenda nil "n"))

  (defun ef-org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'tree))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     (latex . t)
     (ruby . t)))

  (when (display-graphic-p)
    (define-key org-mode-map (kbd "M-RET") 'toggle-frame-fullscreen))

  (ef-shackle '(" *Agenda Commands*" :align below :size .4 :popup t :select t)
              '("*Org Select*" :align below :size .3 :popup t :select t))

  (evil-define-key 'normal org-mode-map ",oc" 'org-toggle-checkbox)
  (evil-define-key 'normal org-mode-map ",o," 'org-priority-up)
  (evil-define-key 'normal org-mode-map ",o." 'org-priority-down)
  (evil-define-key 'normal org-mode-map ",oA" 'ef-org-archive-done-tasks)
  (evil-define-key 'normal org-mode-map ",op" 'org-priority)
  (evil-define-key 'normal org-mode-map ",ot" 'org-todo))

(use-package calendar
  :defer t
  :config
  (ef-shackle '(calendar-mode :align below :size .4 :popup t :select t)))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (ef-add-hook evil-org-mode-hook
    (evil-org-set-key-theme))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

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

(use-package org-bullets
  :after demo-it
  :ensure t
  :config
  (add-hook 'demo-it-mode-hook 'org-bullets-mode))

(use-package expand-region
  :after demo-it
  :ensure t)

(use-package fancy-narrow
  :after demo-it
  :ensure t)

(provide 'pkg-org)
