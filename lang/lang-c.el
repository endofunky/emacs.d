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

  (evil-define-key 'normal 'c++-mode-map ",o" 'ff-find-other-file)
  (evil-define-key 'normal 'c-mode-map ",o" 'ff-find-other-file)

  (defun ef-c-mode-hook ()
    (setq c-default-style "k&r")
    (setq c-basic-offset 8)
    (setq tab-width 8)
    (setq indent-tabs-mode t))

  (add-hook 'c-mode-hook 'ef-c-mode-hook)

  (defun ef-c++-mode-hook ()
    (setq c-default-style "k&r")
    (setq c-basic-offset 2)
    (setq tab-width 2)
    (setq indent-tabs-mode nil)
    (setq company-clang-arguments '("-std=c++11")))

  (add-hook 'c++-mode-hook 'ef-c++-mode-hook)

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
  :config
  (setq ycmd-parse-conditions '(save new-line buffer-focus))
  (setq ycmd-idle-change-delay 0.1)
  (setq url-show-status nil)
  (setq ycmd-request-message-level -1)
  (setq ycmd-server-command '("python"))

  (add-to-list 'ycmd-server-command (expand-file-name "~/.emacs.d/ycmd/ycmd/") t)

  (add-hook 'c++-mode-hook 'ycmd-mode))

(use-package company-ycmd
  :ensure t
  :config
  (setq company-backends
        (remove 'company-clang company-backends))
  (add-to-list 'company-backends 'company-ycmd)
  (company-ycmd-setup))

(provide 'lang-c)
