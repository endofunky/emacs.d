;;; lang-org.el --- `org-mode' configuration -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'core-popup)

(defgroup ef-org nil
  "Endomacs org-mode configuration."
  :group 'org
  :prefix "ef-org-")

(defcustom ef-org-directory (expand-file-name "org" (expand-file-name "Dropbox" (getenv "HOME")))
  "Directory with `org-mode' files."
  :group 'ef-org
  :type 'directory)

(defcustom ef-org-notes-directory (expand-file-name "notes" ef-org-directory)
  "Directory with `org-mode' notes files."
  :group 'ef-org
  :type 'directory)

(defcustom ef-org-roam-directory (expand-file-name "roam" ef-org-directory)
  "Directory with `org-roam' files."
  :group 'ef-org
  :type 'directory)

(defcustom ef-org-notes-file (expand-file-name "todo.org" ef-org-notes-directory)
  "Path to `org-mode' notes file."
  :group 'ef-org
  :type 'file)

(use-package org
  :defer t
  :mode (("\\.\\(org\\|org_archive\\)$" . org-mode))
  :custom
  ;;
  ;; General
  ;;
  (org-adapt-indentation nil)
  (org-archive-location "%s_archive::datetree/* Archived Tasks")
  (org-confirm-babel-evaluate nil)
  (org-deadline-warning-days 7)
  (org-directory ef-org-directory)
  (org-edit-src-content-indentation 0)
  (org-enforce-todo-dependencies t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers nil)
  (org-babel-remove-temporary-stable-directory)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-todo-keywords '((sequence "TODO" "PROG" "|" "DONE")))
  (org-todo-keyword-faces '(("TODO" . error)
                            ("PROG" . warning)
                            ("DONE" . success)))
  ;;
  ;; org-agenda
  ;;
  (org-agenda-files (list ef-org-notes-directory))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-start-on-weekday 1)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-include-deadlines t)
  (org-agenda-include-diary t)
  (org-agenda-block-separator ?―)
  (org-agenda-compact-blocks nil)
  (org-agenda-window-setup 'current-window)
  (org-agenda-time-grid '((daily today require-timed) () "" ""))
  (org-agenda-prefix-format '((agenda  .  " %i %?-15(ef-org-agenda-prefix)%?-12t% s")
                              (todo  . " %i ")
                              (tags  . " %i ")
                              (search . " %i ")))
  :commands (org-capture org-switchb)
  :hook
  (org-mode . flyspell-mode)
  (org-mode . org-indent-mode)
  (org-mode . turn-on-auto-fill)
  (org-agenda-mode . hl-line-mode)
  :general
  (:states 'normal :prefix ef-local-leader
   "o" '(nil :wk "Org")
   "oa" '(ef-org-agenda :wk "Agenda")
   "oc" '(org-capture :wk "Capture")
   "on" '(+org-toggle-notes-file :wk "Toggle notes")
   "or" '(org-roam-node-find :wk "Roam")
   "os" '(org-switchb :wk "Switch buffer"))
  (:states 'normal :keymaps 'org-mode-map
   "RET" 'org-return
   "gd" '(org-open-at-point :wk "Open link")
   "C-t" '(org-mark-ring-goto :wk "Pop back"))
  (:states 'normal :prefix ef-local-leader :keymaps 'org-mode-map
   "o," '(org-priority-up :wk "Priority up")
   "o." '(org-priority-down :wk "Priority down")
   "oA" '(+org-archive-done-tasks :wk "Archive tasks")
   "oe" '(org-export-dispatch :wk "Export")
   "op" '(org-priority :wk "Cycle priority")
   "oR" '(org-roam-buffer-toggle :wk "Toggle roam buffer")
   "ot" '(org-todo :wk "Cycle TODO")

   "Is" '(org-insert-structure-template :wk "Structure Template")

   "l" '(nil :wk "Links")
   "li" '(org-insert-link :wk "Insert")
   "lr" '(org-roam-node-insert :wk "Insert (Roam)")
   "lt" '(org-toggle-link-display :wk "Toggle")

   "t" '(nil :wk "Table")
   "t=" '(org-table-align :wk "Align")
   "tH" '(org-table-move-column-left :wk "Move column left")
   "tJ" '(org-table-move-row-down :wk "Move row down")
   "tK" '(org-table-move-row-up :wk "Move row up")
   "tL" '(org-table-move-column-right :wk "Move column right")
   "td" '(org-table-delete-column :wk "Delete columns")
   "te" '(org-table-edit-field :wk "Edit field")
   "ts" '(org-table-sort-lines :wk "Sort rows")
   "tt" '(org-table-create :wk "Create"))
  :init
  (unless (file-directory-p ef-org-notes-directory)
    (make-directory ef-org-notes-directory))
  :config
  (require 'org-archive)
  (require 'org-capture)
  (require 'ox-md)
  (require 'org-roam)

  (declare-function org-archive-subtree "org-archive")
  (declare-function org-end-of-subtree "org")
  (declare-function org-map-entries "org")
  (declare-function outline-flag-region "outline")
  (declare-function outline-next-heading "outline")
  (declare-function outline-previous-heading "outline")

  (defun ef-org-agenda ()
    "Show org-agenda with with Agenda and TODOs"
    (interactive)
    (org-agenda nil "n"))

  (defun ef-org-agenda-prefix ()
    "Returns the most significant header of the org-outline for the element
to be used in `org-agenda-prefix-format'."
    (let ((x (car (last (org-get-outline-path)))))
      (if x
          (org-format-outline-path (list x))
        "")))

  (defun +org-babel-remove-temporary-stable-directory-a (orig-fun &rest args)
    "Fixes an issue where `kill-emacs-hook' would issue an error when
`org-babel-temporary-stable-directory' is bound but set to nil and a
subsequent `file-exists-p' fails."
    (when (and (boundp 'org-babel-temporary-stable-directory)
	       org-babel-temporary-stable-directory)
      (apply orig-fun args)))

  (advice-add 'org-babel-remove-temporary-stable-directory
              :around #'+org-babel-remove-temporary-stable-directory-a)

  (defun +org-archive-done-tasks ()
    "Archive `org-mode' tasks marked as DONE."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'tree))

  (defun +org-toggle-notes-file ()
    "Toggle notes file `ef-org-notes-file'."
    (interactive)
    (if (string= (buffer-file-name) ef-org-notes-file)
        (if (buffer-modified-p)
            (bury-buffer)
          (kill-buffer))
      (find-file ef-org-notes-file)))

  (defun org-switch-to-buffer-other-window (&rest args)
    (apply 'switch-to-buffer-other-window args))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     (latex . t)
     (ruby . t)))

  (when (display-graphic-p)
    (define-key org-mode-map (kbd "M-RET") 'toggle-frame-fullscreen))

  (poe-popup "*Org Select*")

  (defun org-cycle-hide-drawers (state)
    "Re-hide all drawers after a visibility state change."
    (when (and (derived-mode-p 'org-mode)
               (not (memq state '(overview folded contents))))
      (save-excursion
        (let* ((globalp (memq state '(contents all)))
               (beg (if globalp
                        (point-min)
                      (point)))
               (end (if globalp
                        (point-max)
                      (if (eq state 'children)
                          (save-excursion
                            (outline-next-heading)
                            (point))
                        (org-end-of-subtree t)))))
          (goto-char beg)
          (while (re-search-forward org-drawer-regexp end t)
            (save-excursion
              (beginning-of-line 1)
              (when (looking-at org-drawer-regexp)
                (let* ((start (1- (match-beginning 0)))
                       (limit
                        (save-excursion
                          (outline-next-heading)
                          (point)))
                       (msg (format
                             (concat
                              "org-cycle-hide-drawers:  "
                              "`:END:`"
                              " line missing at position %s")
                             (1+ start))))
                  (if (re-search-forward "^[ \t]*:END:" limit t)
                      (outline-flag-region start (point-at-eol) t)
                    (user-error msg)))))))))))

(use-package org-contrib
  :after org
  :straight (:host github
             :repo "emacsmirror/org-contrib")
  :custom
  (org-eldoc-breadcrumb-separator " → ")
  :config
  (require 'org-eldoc))

(use-package evil-org
  :hook
  (org-mode . evil-org-mode)
  (org-mode . evil-org-set-key-theme)
  :init
  (+add-hook org-agenda-mode-hook
    (require 'evil-org-agenda)
    (declare-function evil-org-agenda-set-keys "evil-org-agenda")
    (evil-org-agenda-set-keys)
    (setq org-super-agenda-header-map nil)))

(use-package org-roam
  :commands (org-roam-node-find org-roam-buffer-toggle)
  :functions (org-roam-setup)
  :init
  (unless (file-directory-p ef-org-roam-directory)
    (make-directory ef-org-roam-directory))
  :hook
  (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :general
  ("<f12>" 'org-roam-node-find)
  :custom
  (org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-directory ef-org-roam-directory)
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (org-roam-setup)
  (poe-popup "*org-roam diagnostics*"))

(use-package org-roam-ui
  :straight (:host github
             :repo "org-roam/org-roam-ui"
             :branch "main"
             :files ("*.el" "out"))
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  :config
  (defun +roam-ui ()
    "Enable `org-roam-ui-mode' and open UI in browser"
    (interactive)
    (unless (bound-and-true-p org-roam-ui-mode)
      (org-roam-ui-mode t))
    (org-roam-ui-open)))

(use-package org-super-agenda
  :after org
  :ensure t
  :custom
  (org-agenda-custom-commands
   '(("n" "Agenda and TODOs"
      ((agenda "" ((org-agenda-span 'day)
                   (org-agenda-overriding-header "")
                   (org-super-agenda-groups
                    '((:name "Today"
                       :and (:time-grid t
                             :not (:habit t))
                       :and (:scheduled today
                             :not (:habit t))
                       :and (:deadline today
                             :not (:habit t))
                       :order 1)
                      (:name "Overdue"
                       :deadline past
                       :order 0)
                      (:name "Due soon"
                       :time-grid t
                       :date t
                       :scheduled t
                       :order 2)))))
       (alltodo "" ((org-agenda-overriding-header "TODOs")
                    (org-super-agenda-groups
                     '((:discard (:habit t :scheduled t :deadline t))
                       (:auto-outline-path t)))))))))
  :config
  (org-super-agenda-mode t))

(use-package ox-gfm
  :after org)

(provide 'lang-org)
