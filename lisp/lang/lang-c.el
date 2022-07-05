;;; lang-c.el --- C/C++/Objective-C configuration -*- lexical-binding: t; -*-
(require 'core-eglot)
(require 'core-popup)
(require 'core-project)
(require 'core-tree-sitter)

(use-package cc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
  :hook
  (c-mode . +cc-mode-enable-lsp)
  (c++-mode . +cc-mode-enable-lsp)
  (objc-mode . +cc-mode-enable-lsp)
  (c-mode-common . c-toggle-hungry-state)
  :custom
  ;; Left-align `#define`s
  (c-electric-pound-behavior '(alignleft))
  :config
  (declare-function +project-root "core-project")

  (+enable-tree-sitter c-mode c++-mode)

  (define-key c-mode-map (kbd "C-M-l") nil)
  (define-key c-mode-map (kbd "C-M-h") nil)
  (define-key c++-mode-map (kbd "C-M-l") nil)
  (define-key c++-mode-map (kbd "C-M-h") nil)

  (+add-hook c-mode-hook
    (setq-local c-default-style "k&r")
    (setq-local c-basic-offset 8)
    (setq-local tab-width 8)
    (setq-local indent-tabs-mode t))

  (+add-hook c++-mode-hook
    (setq-local c-default-style "k&r")
    (setq-local c-basic-offset 2)
    (setq-local tab-width 2)
    (setq-local indent-tabs-mode nil))

  (defun +cc-mode-enable-lsp ()
    "Conditionally enable LSP integration for cc-mode projects."
    (interactive)
    (when (or (file-exists-p (expand-file-name "compile_commands.json" (+project-root)))
              (file-exists-p (expand-file-name ".ccls" (+project-root))))
      (+enable-lsp-maybe)))

  ;; https://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (and (equal major-mode 'c++-mode)
                   (ignore-errors
                     (save-excursion
                       (goto-char (c-langelem-pos langelem))
                       ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                       ;;   and with unclosed brace.
                       (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
              0
            ad-do-it))))

(use-package ruby-style
  :straight nil
  :after (cc-mode)
  :load-path "site-lisp/")

(provide 'lang-c)
