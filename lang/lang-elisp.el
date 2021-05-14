(require 'core-evil)
(require 'core-lib)
(require 'core-projectile)
(require 'core-shackle)

(use-package elisp-mode
  :commands (emacs-lisp-mode lisp-interaction-mode)
  :config
  (evil-define-key 'normal emacs-lisp-mode-map ",r" 'ef-repl-ielm)
  (evil-define-key 'normal lisp-interaction-mode-map ",r" 'ef-repl-ielm)

  (defun ef-elisp-eval-project ()
    (interactive)
    (dolist (buf (buffer-list))
      (let ((project-root (projectile-project-root))
            (buffer-major-mode (with-current-buffer buf
                                 major-mode))
            (buffer-file (buffer-file-name buf)))
        (when (and (or (string= buffer-major-mode "emacs-lisp-mode")
                       (string= buffer-major-mode "lisp-interaction-mode"))
                   ;; Skip files not part of the current project
                   (string-prefix-p project-root buffer-file)
                   ;; Skip special files (such as ./tramp)
                   (string-suffix-p ".el" buffer-file)
                   ;; Skip elpa directory
                   (not (string-prefix-p (expand-file-name "elpa" project-root)
                                         buffer-file)))
          (message "Evaluating: %s" buffer-file)
          (eval-buffer buf)))))

  (defun ef-emacs-lisp-recompile ()
    "Recompile elc file corresponding to `buffer-file-name', if it exists."
    (interactive)
    (when (file-exists-p (byte-compile-dest-file buffer-file-name))
      (emacs-lisp-byte-compile)))

  (ef-add-hook emacs-lisp-mode-hook
    (add-hook 'after-save-hook 'ef-emacs-lisp-recompile nil t)
    (setq-local mode-name "E-λ")))

(use-package elisp-slime-nav
  :ensure t
  :after elisp-mode
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point)
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  ielm-mode-hook
                  lisp-interaction-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

(use-package macrostep
  :ensure t)

(use-package package-lint
  :ensure t
  :defer t
  :commands package-lint-current-buffer)

(use-package eldoc
  :custom
  (eldoc-idle-delay 0.5)
  :config
  ;; Eldoc massively slows down cursor movement. This advice fixes that.
  (advice-add 'eldoc-pre-command-refresh-echo-area :override #'ignore))

(use-package ielm
  :defer t
  :commands (ielm)
  :init
  (ef-define-repl ef-repl-ielm "*ielm*" 'ielm)
  :config
  (evil-define-key 'normal ielm-map ",r" 'ef-repl-ielm)
  (ef-add-hook ielm-mode-hook
    (eldoc-mode t)))

(ef-deflang emacs-lisp
  :after (elisp-mode elisp-slime-nav)
  :maps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  :compile emacs-lisp-byte-compile-and-load
  :compile-nav-jump elisp-slime-nav-find-elisp-thing-at-point
  :compile-nav-pop-back pop-tag-mark
  :doc-apropos counsel-apropos
  :doc-point elisp-slime-nav-describe-elisp-thing-at-point
  :doc-search elisp-index-search
  :eval-all ef-elisp-eval-project
  :eval-buffer eval-buffer
  :eval-expression eval-expression
  :eval-region eval-region
  :eval-sexp eval-print-last-sexp
  :lint-file package-lint-current-buffer
  :macro-expand-all macrostep-expand
  :macro-quit macrostep-collapse-all
  :test-toggle projectile-toggle-between-implementation-and-test
  :xref-apropos xref-find-apropos
  :xref-definitions xref-find-definitions
  :xref-references xref-find-references)

(provide 'lang-elisp)
