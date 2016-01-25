(use-package lisp-mode
  :commands (emacs-lisp-mode lisp-interaction-mode)
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
    (add-hook hook 'turn-on-eldoc-mode))
  :config
  (evil-define-key 'normal emacs-lisp-mode-map ",eb" 'eval-buffer)
  (evil-define-key 'normal emacs-lisp-mode-map ",ee" 'eval-expression)
  (evil-define-key 'normal emacs-lisp-mode-map ",er" 'eval-region)
  (evil-define-key 'normal lisp-interaction-mode-map ",eb" 'eval-buffer)
  (evil-define-key 'normal lisp-interaction-mode-map ",ee" 'eval-expression)
  (evil-define-key 'normal lisp-interaction-mode-map ",er" 'eval-region)

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
    (evil-define-key 'normal emacs-lisp-mode-map ",." 'pop-tag-mark)
    (evil-define-key 'normal lisp-interaction-mode-map ",," 'elisp-slime-nav-find-elisp-thing-at-point)
    (evil-define-key 'normal lisp-interaction-mode-map ",." 'pop-tag-mark))

  (defun ts/emacs-lisp-recompile ()
    "Recompile elc file correspinding to buffer-file-name, if it exists."
    (interactive)
    (when (file-exists-p (byte-compile-dest-file buffer-file-name))
      (emacs-lisp-byte-compile)))

  (defun ts/emacs-lisp-mode-hook ()
    (add-hook 'after-save-hook 'ts/emacs-lisp-recompile nil t)
    (setq mode-name "Emacs λ"))

  (add-hook 'emacs-lisp-mode-hook 'ts/emacs-lisp-mode-hook)

  (defun ts/lisp-mode-hook ()
    (setq mode-name "λ"))

  (add-hook 'lisp-mode-hook 'ts/lisp-mode-hook))

(provide 'lang-lisp)
