(require 'core-lib)
(require 'core-shackle)

(use-package ef-theme
  :demand t
  :straight nil
  :custom
  (custom-safe-themes t)
  (custom-theme-directory (expand-file-name "themes" user-emacs-directory))
  :init
  (require-theme 'ef-theme t)
  :config
  (enable-theme 'ef-theme)

  (ef-add-hook window-configuration-change-hook :fn ef-dim-popups
    (walk-windows (lambda (w)
                    (with-current-buffer (window-buffer w)
                      (if (ef-popup--buffer-p (window-buffer w))
                          (buffer-face-set '(:background "#151617"))
                        (buffer-face-set 'default))))))

  (ef-add-hook prog-mode-hook :fn ef-theme-add-watchwords
    "Highlight FIXME, TODO, and NOCOMMIT in code"
    (font-lock-add-keywords
     nil '(("\\<\\(FIXME\\|BUG\\|XXX\\|TODO\\|NOCOMMIT\\)\\>"
            1 '((:foreground "#cc6666") (:weight bold)) t)))))

(provide 'core-theme)
