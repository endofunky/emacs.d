(require 'core-lib)
(require 'core-shackle)
(require 'xdg)

(defgroup ef-org nil
  "Endomacs org-mode configuration."
  :group 'org
  :prefix "ef-org-")

(defcustom ef-org-directory (expand-file-name "~/org")
  "Directory with `org-mode' files."
  :group 'ef-org
  :type 'directory)

(defcustom ef-org-notes-file (expand-file-name "notes.org" ef-org-directory)
  "Path to `org-mode' notes files."
  :group 'ef-org
  :type 'file)

(defcustom ef-org-bib-directory
  (expand-file-name "bib" ef-org-directory)
  "Directory for `ebib' bibliographies and attached files."
  :group 'ef-org
  :type 'directory)

(use-package org
  :defer t
  :ensure org-plus-contrib
  :ensure t
  :mode (("\\.\\(org\\|org_archive\\)$" . org-mode))
  :custom
  ;;
  ;; General
  ;;
  (org-adapt-indentation nil)
  (org-archive-location "%s_archive::datetree/* Archived Tasks")
  (org-capture-templates
   `(("i" "inbox" entry (file+headline ,ef-org-notes-file "!Inbox")
      "** TODO %?")))
  (org-confirm-babel-evaluate nil)
  (org-deadline-warning-days 7)
  (org-default-notes-file ef-org-notes-file)
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
  ;; Agenda
  ;;
  (org-agenda-files (list ef-org-directory))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-start-on-weekday 1)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-include-deadlines t)
  (org-agenda-include-diary t)
  (org-agenda-block-separator ?―)
  (org-agenda-compact-blocks nil)
  (org-agenda-window-setup 'current-window)
  (org-agenda-time-grid '((daily today require-timed) () "......" ""))
  :commands (org-capture org-switchb)
  :hook
  (org-mode . flyspell-mode)
  (org-mode . org-indent-mode)
  (org-mode . turn-on-auto-fill)
  :general
  (:states 'normal :prefix ef-prefix
   "O" '(ef-org-open-default-notes-files :wk "Org Notes")
   "o" '(nil :wk "Org")
   "oa" '(ef-org-agenda :wk "Agenda")
   "oc" '(org-capture :wk "Capture")
   "oe" '(org-export-dispatch :wk "Export")
   "or" '(org-roam-find-file :wk "Roam")
   "os" '(org-switchb :wk "Switch Buffer")
   "oR" '(org-roam :wk "Roam Display"))
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

  (add-to-list 'org-modules 'org-habit)

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

  (defun ef-org-archive-done-tasks ()
    "Archive `org-mode' tasks marked as DONE."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'tree))

  (defun ef-org-open-default-notes-files ()
    "Open `org-default-notes-file'."
    (interactive)
    (if (string= (buffer-file-name (current-buffer))
                 org-default-notes-file)
        (progn
          (save-buffer)
          (kill-this-buffer))
      (find-file org-default-notes-file)))

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

  ;; Borrowed from doom-emacs
  (ef-add-hook org-agenda-mode-hook :fn ef-org-agenda-align-habit-graphs
    "Align `org-habit' graphs to the right side of the screen."
    (defvar org-habit-preceding-days)
    (defvar org-habit-following-days)

    (require 'org-habit)

    (let* ((total-days (float (+ org-habit-preceding-days org-habit-following-days)))
           (preceding-days-ratio (/ org-habit-preceding-days total-days))
           (graph-width (floor (* (window-width) 0.3)))
           (preceding-days (floor (* graph-width preceding-days-ratio)))
           (following-days (- graph-width preceding-days))
           (graph-column (- (window-width) (+ preceding-days following-days)))
           (graph-column-adjusted (if (> graph-column 30)
                                      (- graph-column 2)
                                    nil)))
      (setq-local org-habit-preceding-days preceding-days)
      (setq-local org-habit-following-days following-days)
      (setq-local org-habit-graph-column graph-column-adjusted)))

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

(use-package org-super-agenda
  :after org
  :ensure t
  :custom
  (org-agenda-custom-commands
   '(("n" "Agenda and TODOs"
      ((agenda "" ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '((:name "Today"
                       :and (:time-grid t
                             :not (:habit t))
                       :and (:deadline today
                             :not (:habit t))
                       :and (:scheduled today
                             :not (:habit t))
                       :order 3)
                      (:name "Overdue"
                       :deadline past
                       :order 1)
                      (:name "Habits"
                       :habit t
                       :order 2)
                      (:auto-outline-path t
                       :time-grid t
                       :date t
                       :scheduled t
                       :order 4)))))
       (alltodo "" ((org-agenda-overriding-header "TODOs")
                    (org-super-agenda-groups
                     '((:discard (:habit t :scheduled t :deadline t))
                       (:auto-outline-path t)))))))))
  :config
  (org-super-agenda-mode t))

(use-package evil-org
  :ensure t
  :hook
  (org-mode . evil-org-mode)
  (org-mode . evil-org-set-key-theme)
  :init
  (ef-add-hook org-agenda-mode-hook
    (require 'evil-org-agenda)
    (declare-function evil-org-agenda-set-keys "evil-org-agenda")
    (evil-org-agenda-set-keys)
    (setq org-super-agenda-header-map nil)))

(use-package org-roam
  :ensure t
  :init
  (unless (file-directory-p (expand-file-name "roam" ef-org-directory))
    (make-directory (expand-file-name "roam" ef-org-directory)))
  :hook
  (after-init . org-roam-mode)
  (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :general
  ("<f12>" 'org-roam-find-file)
  :custom
  (org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-directory (expand-file-name "roam" ef-org-directory))
  :config
  (ef-add-popup "*org-roam diagnostics*"))

(use-package ebib
  :ensure t
  :commands ebib
  :hook
  (ebib-entry-mode . visual-line-mode)
  :custom
  (ebib-index-window-size 20)
  (ebib-name-transform-function #'ef-ebib-name-transform-function)
  (ebib-import-directory (or (xdg-user-dir "DOWNLOAD")
                             (expand-file-name "~/Downloads/")))
  (ebib-file-associations nil)
  (ebib-file-search-dirs `(,(expand-file-name "ebib/files/" org-directory)))
  (ebib-notes-directory (expand-file-name "ebib/notes/" org-directory))
  (ebib-preload-bib-files `(,(expand-file-name "ebib/bibliography.bib" org-directory)))
  :config
  (defun ef-ebib-name-transform-function (key)
    (replace-regexp-in-string "\\/" "_" key)))

(use-package bibtex-completion
  :ensure t
  :after ebib
  :custom
  (bibtex-completion-bibliography ebib-preload-bib-files))

(provide 'lang-org)
