(use-package cc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
  :config
  (defun ef-cc-mode-hook ()
    (sp-with-modes '(c-mode c++-mode cc-mode)
      (sp-local-pair "#include <" ">")
      (sp-local-pair "[" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ef-sp-create-newline-and-enter-sexp "RET")))))

  (add-hook 'c++-mode-hook 'ef-cc-mode-hook)
  (add-hook 'c-mode-hook 'ef-cc-mode-hook)

  (evil-define-key 'normal ",o" 'c++-mode-map 'ff-find-other-file)
  (evil-define-key 'normal 'c-mode-map ",o" 'ff-find-other-file)

  (defun ef-c-mode-hook ()
    (setq-local c-default-style "k&r")
    (setq-local c-basic-offset 8)
    (setq-local tab-width 8)
    (setq-local indent-tabs-mode t))

  (add-hook 'c-mode-hook 'ef-c-mode-hook)

  (defun ef-c++-mode-hook ()
    (setq-local c-default-style "k&r")
    (setq-local c-basic-offset 2)
    (setq-local tab-width 2)
    (setq-local indent-tabs-mode nil)
    (setq-local company-clang-arguments '("-std=c++11")))

  (add-hook 'c++-mode-hook 'ef-c++-mode-hook)

  ;; https://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq-local ad-return-value
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
  :after cc-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :config
  (modern-c++-font-lock-global-mode t))

(use-package ycmd
  :ensure t
  :custom
  (ycmd-request-message-level -1)
  (ycmd-parse-conditions '(save buffer-focus new-line mode-enabled))
  (ycmd-idle-change-delay 0)
  (ycmd-force-semantic-completion nil)
  (ycmd-server-command '("python"))
  :config
  (add-to-list 'ycmd-server-command (expand-file-name "~/.emacs.d/ycmd/ycmd/") t)
  (add-hook 'c++-mode-hook 'ycmd-mode))

(use-package company-ycmd
  :ensure t
  :custom
  (company-backends (remove 'company-clang company-backends))
  (company-ycmd-request-sync-timeout 1.0)
  :config
  (add-to-list 'company-backends 'company-ycmd)
  (company-ycmd-setup))

(use-package bazel-mode
  :ensure t
  :mode ("BUILD\\|WORKSPACE\\|CROSSTOOL\\|\\.bazel\\'" . bazel-mode)
  :config
  (defun ef-bazel-mode-hook ()
    (add-hook 'before-save-hook 'bazel-format nil t))
  (add-hook 'bazel-mode-hook 'ef-bazel-mode-hook))

(use-package clang-format
  :ensure t
  :config
  (fset 'c-indent-region 'clang-format-region)
  (evil-define-key 'normal c++-mode-map (kbd ", TAB") 'clang-format-region)
  (evil-define-key 'normal c-mode-map (kbd ", TAB") 'clang-format-region))

(provide 'lang-c)
