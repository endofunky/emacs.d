(require 'core-shackle)
(require 'xdg)

(defgroup ef-org nil
  "Endomacs org-mode configuration."
  :group 'org
  :prefix "ef-org-")

(defcustom ef-org-directory (expand-file-name "org" (getenv "HOME"))
  "Directory with `org-mode' files."
  :group 'ef-org
  :type 'directory)

(defcustom ef-org-bib-directory
  (expand-file-name "bib" ef-org-directory)
  "Directory for `ebib' bibliographies and attached files."
  :group 'ef-org
  :type 'directory)

(defvar org-habit-preceding-days)
(defvar org-habit-following-days)

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
  (org-hide-emphasis-markers t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation nil)
  (org-src-tab-acts-natively t)
  ;;
  ;; org-habit
  ;;
  (org-habit-following-days 2)
  :commands (org-capture org-switchb)
  :hook
  (org-mode . flyspell-mode)
  (org-mode . turn-on-auto-fill)
  :general
  (:states 'normal :prefix ef-prefix
   "o" '(nil :wk "Org")
   "oc" '(org-capture :wk "Capture")
   "oe" '(org-export-dispatch :wk "Export")
   "or" '(org-roam-node-find :wk "Roam")
   "os" '(org-switchb :wk "Switch Buffer")
   "oR" '(org-roam-buffer-toggle :wk "Toggle Roam Buffer"))
  (:states 'normal :prefix ef-prefix :keymaps 'org-mode-map
   "," '(org-open-at-point :wk "Open Link")
   "." '(org-mark-ring-goto :wk "Pop Back")
   "o" '(nil :wk "Org")
   "o," '(org-priority-up :wk "Priority Up")
   "o." '(org-priority-down :wk "Priority Down")
   "oA" '(ef-org-archive-done-tasks :wk "Archive Done Tasks")
   "op" '(org-priority :wk "Cycle Priority")
   "ot" '(org-todo :wk "Cycle TODO")

   "l" '(nil :wk "Org Links")
   "li" '(org-insert-link :wk "Insert Link")
   "lr" '(org-roam-insert :wk "Insert Roam Link")
   "lt" '(org-toggle-link-display :wk "Toggle Links")

   "t" '(nil :wk "Org Table")
   "t=" '(org-table-align :wk "Align")
   "tH" '(org-table-move-column-left :wk "Move Column Left")
   "tJ" '(org-table-move-row-down :wk "Move Row Down")
   "tK" '(org-table-move-row-up :wk "Move Row Up")
   "tL" '(org-table-move-column-right :wk "Move Column Right")
   "td" '(org-table-delete-column :wk "Delete Columns")
   "te" '(org-table-edit-field :wk "Edit Field")
   "ts" '(org-table-sort-lines :wk "Sort Rows")
   "tt" '(org-table-create :wk "Create"))
  :config
  (require 'org-archive)
  (require 'org-capture)
  (require 'org-install)
  (require 'ox-md)
  (require 'org-roam)

  (declare-function org-archive-subtree "org-archive")
  (declare-function org-end-of-subtree "org")
  (declare-function org-format-outline-path "org")
  (declare-function org-get-outline-path "org")
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

(use-package evil-org
  :hook
  (org-mode . evil-org-mode)
  (org-mode . evil-org-set-key-theme))

(use-package org-roam
  :commands (org-roam-node-find org-roam-buffer-toggle)
  :defines (org-roam-v2-ack)
  :functions (org-roam-setup)
  :init
  (unless (file-directory-p (expand-file-name "roam" ef-org-directory))
    (make-directory (expand-file-name "roam" ef-org-directory)))
  (setq org-roam-v2-ack t)
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
