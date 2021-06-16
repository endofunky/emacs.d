(require 'core-shackle)

(defgroup ef-org nil
  "Endomacs org-mode configuration."
  :group 'org
  :prefix "ef-org-")

(defcustom ef-org-directory (expand-file-name "~/org")
  "Directory with Org files."
  :group 'ef-org
  :type 'directory)

(defcustom ef-org-bib-directory
  (expand-file-name "bib" ef-org-directory)
  "Directory with Org files."
  :group 'ef-org
  :type 'directory)

(use-package org
  :defer t
  :ensure org-plus-contrib
  :ensure t
  :mode (("\\.\\(org\\|org_archive\\)$" . org-mode))
  :custom
  (org-directory ef-org-directory)
  (org-adapt-indentation nil)
  (org-confirm-babel-evaluate nil)
  (org-deadline-warning-days 7)
  (org-edit-src-content-indentation 0)
  (org-enforce-todo-dependencies t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-log-done 'time)
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation nil)
  (org-src-tab-acts-natively t)
  :commands (org-capture org-switchb)
  :hook
  (org-mode . flyspell-mode)
  (org-mode . org-indent-mode)
  (org-mode . turn-on-auto-fill)
  :general
  (:states 'normal :prefix ef-prefix
   "," '(org-open-at-point :wk "Open Link")
   "." '(org-mark-ring-goto :wk "Pop Back")
   "o" '(nil :wk "Org")
   "oc" '(org-capture :wk "Capture")
   "or" '(org-roam-find-file :wk "Roam")
   "os" '(org-switchb :wk "Switch Buffer")
   "oR" '(org-roam :wk "Roam Display"))
  (:states 'normal :prefix ef-prefix :keymaps 'org-mode-map
   "o" '(nil :wk "Org")
   "oc" '(org-toggle-checkbox :wk "Toggle Checkbox")
   "o," '(org-priority-up :wk "Priority Up")
   "o." '(org-priority-down :wk "Priority Down")
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
  (require 'org-capture)
  (require 'org-install)
  (require 'ox-md)

  (declare-function outline-flag-region "outline")
  (declare-function outline-next-heading "outline")
  (declare-function org-end-of-subtree "org")

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
  :ensure t
  :after org
  :hook
  (org-mode . evil-org-mode)
  (org-mode . evil-org-set-key-theme))

(use-package org-roam
  :ensure t
  :init
  (unless (file-directory-p (expand-file-name "~/org/roam/"))
    (make-directory (expand-file-name "~/org/roam/")))
  :hook
  (after-init . org-roam-mode)
  (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :general
  ("<f12>" 'org-roam-find-file)
  :custom
  (org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-directory (expand-file-name "~/org/roam/"))
  :config
  (ef-add-popup "*org-roam diagnostics*"))

(use-package ebib
  :ensure t
  :commands ebib
  :hook
  (ebib-entry-mode . visual-line-mode)
  :custom
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
