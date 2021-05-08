(require 'core-evil)
(require 'core-lib)
(require 'core-shackle)

(use-package elisp-mode
  :commands (emacs-lisp-mode lisp-interaction-mode)
  :config
  (evil-define-key 'normal emacs-lisp-mode-map ",r" 'ef-repl-ielm)
  (evil-define-key 'normal emacs-lisp-mode-map ",m" 'counsel-apropos)
  (evil-define-key 'normal lisp-interaction-mode-map ",m" 'counsel-apropos)
  (evil-define-key 'normal lisp-interaction-mode-map ",r" 'ef-repl-ielm)

  (defun ef-emacs-lisp-recompile ()
    "Recompile elc file corresponding to `buffer-file-name', if it exists."
    (interactive)
    (when (file-exists-p (byte-compile-dest-file buffer-file-name))
      (emacs-lisp-byte-compile)))

  (ef-add-hook emacs-lisp-mode-hook
    (add-hook 'after-save-hook 'ef-emacs-lisp-recompile nil t)
    (setq-local mode-name "E-λ")))

(use-package elisp-slime-nav
  :defer t
  :ensure t
  :commands (elisp-slime-nav-mode)
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  ielm-mode-hook
                  lisp-interaction-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode))
  :config
  (evil-define-key 'normal emacs-lisp-mode-map ",," 'elisp-slime-nav-find-elisp-thing-at-point)
  (evil-define-key 'normal lisp-interaction-mode-map ",," 'elisp-slime-nav-find-elisp-thing-at-point))

(use-package macrostep
  :ensure t
  :config
  (evil-define-key 'normal emacs-lisp-mode-map ",xe" 'macrostep-expand)
  (evil-define-key 'normal lisp-interaction-mode-map ",xe" 'macrostep-expand)
  (evil-define-key 'normal macrostep-keymap "q" 'macrostep-collapse-all))

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
  (evil-define-key 'normal ielm-map ",m" 'counsel-apropos)
  (ef-add-hook ielm-mode-hook
    (eldoc-mode t)))

(ef-deflang emacs-lisp
  :after elisp-mode
  :maps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  :compile emacs-lisp-byte-compile-and-load
  :eval-buffer eval-buffer
  :eval-expression eval-expression
  :eval-region eval-region
  :eval-sexp eval-print-last-sexp
  :lint-file package-lint-current-buffer)

(provide 'lang-elisp)
