(use-package lispy
  :ensure t
  :diminish t
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
  :diminish t
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


(provide 'pkg-lispy)
