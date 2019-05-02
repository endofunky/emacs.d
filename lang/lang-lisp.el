(use-package lisp-mode
  :mode (("\\.cl\\'" . ruby-mode)
         ("\\.lisp\\'" . ruby-mode)
         ("\\.sbclrc\\'" . ruby-mode))
  :config
  (ef-add-hook lisp-mode-hook
    (slime-mode t)
    (setq-local mode-name "λ")))

(use-package slime-company
  :ensure t
  :defer t
  :commands slime-company)

(use-package slime
  :ensure t
  :defer t
  :commands (slime slime-lisp-mode-hook slime-mode)
  :custom
  (inferior-lisp-program "sbcl")
  (slime-net-coding-system 'utf-8-unix)
  (slime-complete-symbol*-fancy t)
  (slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  :config
  (setq slime-contribs
        '(slime-fancy slime-asdf slime-quicklisp slime-cl-indent))

  (slime-setup '(slime-fancy slime-asdf slime-quicklisp slime-company))

  (ef-shackle '(sldb-mode :align bottom :size .4 :popup t :select t)
              '("*slime-compilation*" :align bottom :size .4 :popup t :select t)
              '("*slime-description*" :align bottom :size .4 :popup t :select t)
              '("*slime-apropos*" :align bottom :size .4 :popup t :select t))

  (evil-define-key 'normal slime-mode-map ",," 'slime-edit-definition)
  (evil-define-key 'normal slime-mode-map ",." 'slime-pop-find-definition-stack)
  (evil-define-key 'normal slime-mode-map ",eb" 'slime-eval-buffer)
  (evil-define-key 'visual slime-mode-map ",er" 'slime-eval-region)
  (evil-define-key 'normal slime-mode-map ",ed" 'slime-eval-defun)
  (evil-define-key 'normal slime-mode-map ",cc" 'slime-compile-and-load-file)
  (evil-define-key 'region slime-mode-map ",cr" 'slime-compile-region)

  (ef-define-repl ef-repl-slime "*slime-repl sbcl*" 'slime-repl)
  (evil-define-key 'normal lisp-mode-map ",r" 'ef-repl-slime)
  (evil-define-key 'normal slime-repl-mode-map ",r" 'ef-repl-slime)

  (evil-define-key 'insert slime-repl-mode-map (kbd "<up>") 'slime-repl-previous-input)
  (evil-define-key 'insert slime-repl-mode-map (kbd "<down>") 'slime-repl-next-input))

(use-package elisp-mode
  :commands (emacs-lisp-mode lisp-interaction-mode)
  :config
  (evil-define-key 'normal emacs-lisp-mode-map ",eb" 'eval-buffer)
  (evil-define-key 'normal emacs-lisp-mode-map ",ee" 'eval-expression)
  (evil-define-key 'visual emacs-lisp-mode-map ",er" 'eval-region)
  (evil-define-key 'normal emacs-lisp-mode-map ",r" 'ef-repl-ielm)
  (evil-define-key 'normal emacs-lisp-mode-map ",cc" 'emacs-lisp-byte-compile-and-load)
  (evil-define-key 'normal emacs-lisp-mode-map ",m" 'apropos)
  (evil-define-key 'normal lisp-interaction-mode-map ",m" 'apropos)
  (evil-define-key 'normal lisp-interaction-mode-map ",eb" 'eval-buffer)
  (evil-define-key 'normal lisp-interaction-mode-map ",ee" 'eval-expression)
  (evil-define-key 'visual lisp-interaction-mode-map ",er" 'eval-region)
  (evil-define-key 'normal lisp-interaction-mode-map ",r" 'ef-repl-ielm)
  (evil-define-key 'normal lisp-interaction-mode-map ",cc" 'emacs-lisp-byte-compile-and-load)
  (evil-define-key 'normal ielm-map ",r" 'ef-repl-ielm)
  (evil-define-key 'normal ielm-map ",m" 'apropos)

  (defun ef-emacs-lisp-recompile ()
    "Recompile elc file corresponding to buffer-file-name, if it exists."
    (interactive)
    (when (file-exists-p (byte-compile-dest-file buffer-file-name))
      (emacs-lisp-byte-compile)))

  (ef-add-hook emacs-lisp-mode-hook
    (add-hook 'after-save-hook 'ef-emacs-lisp-recompile nil t)
    (setq-local mode-name "Emacs λ")))

(use-package ielm
  :defer t
  :commands (ielm)
  :init
  (ef-define-repl ef-repl-ielm "*ielm*" 'ielm)
  :config
  (ef-add-hook ielm-mode-hook
    (eldoc-mode t)))

(use-package elisp-slime-nav
  :defer t
  :ensure t
  :diminish elisp-slime-nav-mode
  :commands (elisp-slime-nav-mode)
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
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
  :init
  (evil-define-key 'normal emacs-lisp-mode-map ",clf" 'package-lint-current-buffer)
  :commands package-lint-current-buffer
  :config
  (ef-shackle '("*Package-Lint*" :align bottom :size .4 :popup t :select t)))

(use-package eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-idle-delay 0.5)
  :config

  ;; Eldoc massively slows down cursor movement. This advice fixes that.
  (advice-add 'eldoc-pre-command-refresh-echo-area :override #'ignore))

(use-package lispy
  :ensure t
  :diminish lispy-mode 
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (racket-mode . lispy-mode)
         (hy-mode . lispy-mode)
         (lfe-mode . lispy-mode)
         (clojure-mode . lispy-mode))
  :config
  (lispy-set-key-theme '(paredit))
  (ef-add-hook lispy-mode-hook
    (if (fboundp 'turn-off-smartparens-mode)
        (turn-off-smartparens-mode))))

(use-package lispyville
  :ensure t
  :diminish lispyville-mode
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '((operators normal)
     c-w
     commentary
     additional-wrap
     slurp/barf-cp))
  (evil-define-key 'normal lispyville-mode-map "#" #'lispyville-comment-or-uncomment-line)
  (evil-define-key 'normal lispyville-mode-map "\\" #'lispyville-comment-or-uncomment-line)
  (evil-define-key 'visual lispyville-mode-map "#" #'lispyville-comment-or-uncomment)
  (evil-define-key 'visual lispyville-mode-map "\\" #'lispyville-comment-or-uncomment))

(provide 'lang-lisp)
