(use-package lisp-mode
  :mode (("\\.cl\\'" . lisp-mode)
         ("\\.lisp\\'" . lisp-mode)
         ("\\.sbclrc\\'" . lisp-mode))
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

  (ef-add-popup 'sldb-mode)
  (ef-add-popup "*slime-compilation*")
  (ef-add-popup "*slime-description*" :ephemeral t)
  (ef-add-popup "*slime-apropos*" :ephemeral t)

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

(provide 'lang-lisp)
