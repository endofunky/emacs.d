;;; core-theme.el --- Eye candy -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'core-popup)

(use-package alpine-night-theme
  :load-path "site-lisp/themes/"
  :when (or (display-graphic-p)
            (= (tty-display-color-cells) 16777216))
  :straight nil
  :custom
  (custom-safe-themes t)
  :init
  (add-to-list 'custom-theme-load-path
               (expand-file-name "themes" user-emacs-directory))
  :config
  (load-theme 'alpine-night t))

(+add-hook prog-mode-hook :fn +add-watchwords-h
  "Highlight FIXME, TODO, and NOCOMMIT in code"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|BUG\\|XXX\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#cc6666") (:weight bold)) t))))

(defun +reload-theme ()
  "Reload currently enabled theme."
  (interactive)
  (when-let ((theme (car custom-enabled-themes)))
    (disable-theme theme)
    (load-theme theme t)))

(use-package uniline
  :demand t
  :straight nil
  :load-path "site-lisp/"
  :commands (uniline-mode)
  :config
  (uniline-mode t))

(provide 'core-theme)
