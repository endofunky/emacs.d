(require 'core-shackle)

(defgroup ef-org nil
  "Endomacs org-mode configuration."
  :group 'org
  :prefix "ef-org-")

(defcustom ef-org-directory (expand-file-name "org" (getenv "HOME"))
  "Directory with `org-mode' files."
  :group 'ef-org
  :type 'directory)

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
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  ;;
  ;; org-habit
  ;;
  (org-habit-following-days 2)
  :commands (org-capture org-switchb)
  :hook
  (org-mode . flyspell-mode)
  (org-mode . org-indent-mode)
  (org-mode . turn-on-auto-fill)
  :general
  (:states 'normal :prefix ef-prefix
   "o" '(nil :wk "Org")
   "oc" '(org-capture :wk "Capture")
   "or" '(org-roam-node-find :wk "Roam")
   "os" '(org-switchb :wk "Switch buffer"))
  (:states 'normal :prefix ef-prefix :keymaps 'org-mode-map
   "," '(org-open-at-point :wk "Open link")
   "." '(org-mark-ring-goto :wk "Pop back")
   "o" '(nil :wk "org-mode")
   "o," '(org-priority-up :wk "Priority up")
   "o." '(org-priority-down :wk "Priority down")
   "oA" '(ef-org-archive-done-tasks :wk "Archive tasks")
   "oe" '(org-export-dispatch :wk "Export")
   "op" '(org-priority :wk "Cycle priority")
   "oR" '(org-roam-buffer-toggle :wk "Toggle roam buffer")
   "ot" '(org-todo :wk "Cycle TODO")

   "l" '(nil :wk "org-mode links")
   "li" '(org-insert-link :wk "Insert link")
   "lr" '(org-roam-insert :wk "Insert roam link")
   "lt" '(org-toggle-link-display :wk "Toggle links")

   "t" '(nil :wk "org-table")
   "t=" '(org-table-align :wk "align")
   "tH" '(org-table-move-column-left :wk "Move column left")
   "tJ" '(org-table-move-row-down :wk "Move row down")
   "tK" '(org-table-move-row-up :wk "Move row up")
   "tL" '(org-table-move-column-right :wk "Move column right")
   "td" '(org-table-delete-column :wk "Delete columns")
   "te" '(org-table-edit-field :wk "Edit field")
   "ts" '(org-table-sort-lines :wk "Sort rows")
   "tt" '(org-table-create :wk "Create"))
  :config
  (require 'org-archive)
  (require 'org-capture)
  (require 'org-install)
  (require 'ox-md)
  (require 'org-roam)

  (declare-function org-archive-subtree "org-archive")
  (declare-function org-end-of-subtree "org")
  (declare-function org-map-entries "org")
  (declare-function outline-flag-region "outline")
  (declare-function outline-next-heading "outline")
  (declare-function outline-previous-heading "outline")

  (defun ef-org-archive-done-tasks ()
    "Archive `org-mode' tasks marked as DONE."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'tree))

  (defun org-switch-to-buffer-other-window (&rest args)
    (apply 'switch-to-buffer-other-window args))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     (latex . t)
     (ruby . t)))

  (when window-system
    (define-key org-mode-map (kbd "M-RET") 'toggle-frame-fullscreen))

  (ef-add-popup "*Org Select*")

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
  (org-mode . evil-org-set-key-theme))

(use-package org-roam
  :commands (org-roam-node-find org-roam-buffer-toggle)
  :functions (org-roam-setup)
  :init
  (unless (file-directory-p (expand-file-name "roam" ef-org-directory))
    (make-directory (expand-file-name "roam" ef-org-directory)))
  :hook
  (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :general
  ("<f12>" 'org-roam-node-find)
  :custom
  (org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-directory (expand-file-name "roam" ef-org-directory))
  :config
  (org-roam-setup)
  (ef-shackle '("*org-roam*" :align right :size .5 :popup t :select t :float t))
  (ef-add-popup "*org-roam diagnostics*"))

(use-package ox-gfm
  :after org)

(provide 'lang-org)
