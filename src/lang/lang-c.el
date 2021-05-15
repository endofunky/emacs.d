(use-package cc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
  :config
  (define-key c-mode-map (kbd "C-M-l") nil)
  (define-key c-mode-map (kbd "C-M-h") nil)
  (define-key c++-mode-map (kbd "C-M-l") nil)
  (define-key c++-mode-map (kbd "C-M-h") nil)

  (ef-add-hook (c++-mode-hook c-mode-hook) :fn ef-cc-mode-hook
    (sp-with-modes '(c-mode c++-mode cc-mode)
      (sp-local-pair "#include <" ">")
      (sp-local-pair "[" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))))

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

(use-package company-c-headers
  :after cc-mode
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package cmake-mode
  :ensure t
  :commands cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package modern-cpp-font-lock
  :ensure t
  :after c++-mode
  :config
  (modern-c++-font-lock-global-mode t))

(use-package ccls
  :ensure t
  :custom
  (ccls-sem-highlight-method nil)
  :config
  (ef-add-hook (c-mode-hook c++-mode-hook objc-mode-hook) :fn ef-c-mode-lsp-hook :interactive t
    (direnv-update-directory-environment)
    (require 'ccls)
    (if (and (file-exists-p (expand-file-name "compile_commands.json" (projectile-project-root)))
             (locate-file "ccls" exec-path))
        (lsp))))


(use-package ruby-style
  :after (cc-mode)
  :load-path "vendor/ruby-style.el")

(provide 'lang-c)
