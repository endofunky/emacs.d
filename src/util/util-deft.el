(use-package deft
  :general
  ("<f12>" 'deft)
  (:states 'normal :keymaps 'deft-mode-map
   "r" 'deft-rename-file
   "a" 'deft-new-file
   "A" 'deft-new-file-named
   "d" 'deft-delete-file
   "D" 'deft-archive-file
   "q" 'kill-current-buffer
   "R" 'deft-refresh
   "s" 'deft-filter)
  :init
  (setq deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  :custom
  (deft-auto-save-interval -1.0)
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-extensions '("org" "md" "txt"))
  (deft-default-extension "org")
  :config
  (defun ef-deft ()
    "Display *Deft* buffer and load files."
    (interactive)
    (let ((buffer (get-create-buffer deft-buffer)))
      (display-buffer buffer)
      (with-current-buffer buffer
        (if (not (eq major-mode 'deft-mode))
            (deft-mode)))))
  (evil-set-initial-state 'deft-mode 'insert))

(provide 'util-deft)
