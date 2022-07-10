;;; core-theme.el --- Eye candy -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'core-popup)

(use-package efdark-theme
  :demand t
  :load-path "site-lisp/themes/"
  :straight nil
  :custom
  (custom-safe-themes t)
  :init
  (add-to-list 'custom-theme-load-path
               (expand-file-name "themes" user-emacs-directory))
  :config
  (load-theme 'efdark t)

  (defun +reload-theme ()
    "Reload theme."
    (interactive)
    (disable-theme 'efdark)
    (load-theme 'efdark t)))

(+add-hook prog-mode-hook :fn +add-watchwords-h
  "Highlight FIXME, TODO, and NOCOMMIT in code"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|BUG\\|XXX\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#cc6666") (:weight bold)) t))))

(use-package uniline
  :demand t
  :straight nil
  :load-path "site-lisp/"
  :commands (uniline-mode)
  :config
  (uniline-mode t))

(provide 'core-theme)
