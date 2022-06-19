(require 'core-lsp)
(require 'core-shackle)
(require 'core-project)

(use-package cc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
  :hook
  (c-mode . ef-cc-mode-enable-lsp)
  (c++-mode . ef-cc-mode-enable-lsp)
  (objc-mode . ef-cc-mode-enable-lsp)
  (c-mode-common . c-toggle-hungry-state)
  :custom
  ;; Left-align `#define`s
  (c-electric-pound-behavior (quote (alignleft)))
  :config
  (declare-function ef-project-root "core-project")

  (define-key c-mode-map (kbd "C-M-l") nil)
  (define-key c-mode-map (kbd "C-M-h") nil)
  (define-key c++-mode-map (kbd "C-M-l") nil)
  (define-key c++-mode-map (kbd "C-M-h") nil)

  (ef-add-hook c-mode-hook
    (setq-local c-default-style "k&r")
    (setq-local c-basic-offset 8)
    (setq-local tab-width 8)
    (setq-local indent-tabs-mode t))

  (ef-add-hook c++-mode-hook
    (setq-local c-default-style "k&r")
    (setq-local c-basic-offset 2)
    (setq-local tab-width 2)
    (setq-local indent-tabs-mode nil))

  (defun ef-cc-mode-enable-lsp ()
    "Conditionally enable LSP integration for cc-mode projects."
    (interactive)
    (when (or (file-exists-p (expand-file-name "compile_commands.json" (ef-project-root)))
              (file-exists-p (expand-file-name ".ccls" (ef-project-root))))
      (ef-enable-lsp-maybe)))

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
  :load-path "vendor/")

(ef-deflang c++
  :after (cc-mode)
  :compile
  (lambda ()
    (interactive)
    (compile "make -k"))
  :compile-and-run
  (lambda ()
    (interactive)
    (compile "make -k run" t)))

(provide 'lang-c)
