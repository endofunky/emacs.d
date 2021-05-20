(require 'core-shackle)

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
  :general
  (:states 'normal :prefix ef-prefix
           "o" '(nil :which-key "Org")
           "oa" '(ef-org-agenda :which-key "Agenda")
           "oc" '(org-capture :which-key "Capture")
           "oo" '(ef-org-notes :which-key "Notes"))
  (:states 'normal :prefix ef-prefix :keymaps 'org-mode-map
           "o" '(nil :which-key "Org")
           "oc" '(org-toggle-checkbox :which-key "Toggle Checkbox")
           "o," '(org-priority-up :which-key "Priority Up")
           "o." '(org-priority-down :which-key "Priority Down")
           "oA" '(ef-org-archive-done-tasks :which-key "Archive Done Tasks")
           "op" '(org-priority :which-key "Cycle Priority")
           "ot" '(org-todo :which-key "Cycle TODO"))
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

  (when window-system
    (define-key org-mode-map (kbd "M-RET") 'toggle-frame-fullscreen))

  (ef-add-popup " *Agenda Commands*")
  (ef-add-popup "*Org Select*"))

(use-package calendar
  :defer t
  :config
  (ef-add-popup 'calendar-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook
  (org-mode . evil-org-mode)
  (org-mode . evil-org-set-key-theme)
  :custom
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'lang-org)
