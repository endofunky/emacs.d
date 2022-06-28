;;; core-dired.el --- Directory editor -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package dired
  :defer t
  :straight nil
  :commands (dired)
  :general
  (:states 'normal :prefix ef-leader
   "D" '(ef-dired-here :wk "dired"))
  (:keymaps 'dired-mode-map
   "<return>" 'dired-find-alternate-file)
  (:states 'normal :prefix ef-leader :keymaps 'dired-mode-map
   "D" 'bury-buffer)
  :init
  (defun ef-dired-here ()
    "Open `dired' in current directory."
    (interactive)
    (dired "."))
  :custom
  (dired-auto-revert-buffer t)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  :config
  (ef-shackle '(dired-mode :align right :size .5 :popup nil
                          :select t :float t :inhibit-window-quit t))

  ;; As opposed to what the documentation says, `dired-find-alternate-file' is
  ;; actually more intuitive for <return> than the default, `find-file', so we
  ;; enable it here.
  (put 'dired-find-alternate-file 'disabled nil))

(provide 'core-dired)
