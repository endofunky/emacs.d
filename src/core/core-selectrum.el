(use-package selectrum
  :straight t
  :demand t
  :commands selectrum-mode
  :custom
  (selectrum-max-window-height .2)
  (orderless-style-dispatchers '(ef-selectrum-without-if-bang
                                 ef-selectrum-with-if-equals))
  :config
  (defun ef-selectrum-with-if-equals (pattern index total)
    "Selectrum style dispatcher to literal match results using equal sign."
    (when (string-prefix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 1))))

  (defun ef-selectrum-without-if-bang (pattern index total)
    "Selectrum style dispatcher to discard results matching literal following
exclamation mark."
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1))))

  (selectrum-mode t))

(use-package orderless
  :after selectrum
  :straight t
  :custom
  (selectrum-extend-current-candidate-highlight t)
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (orderless-matching-styles '(orderless-regexp
                               orderless-flex))
  (completion-styles '(orderless)))

(use-package marginalia
  :after selectrum
  :straight t
  :demand t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light))
  :config
  (add-to-list 'marginalia-command-categories
               '(projectile-find-file . project-file))
  (add-to-list 'marginalia-command-categories
               '(projectile-recentf . project-file))
  (add-to-list 'marginalia-command-categories
               '(projectile-switch-project . project-file))
  (add-to-list 'marginalia-command-categories
               '(projectile-switch-to-buffer . buffer))
  (add-to-list 'marginalia-command-categories
               '(ef-projectile-find-file . project-file))

  (marginalia-mode t))

(use-package consult
  :straight t
  :demand t
  :custom
  (consult-preview-key nil)
  (consult-project-root-function #'projectile-project-root)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :general
  ("M-y" 'consult-yank-from-kill-ring)
  ("C-s" 'consult-line)
  (:states 'normal :prefix ef-prefix
   "/" '(consult-ripgrep :wk "Grep (rg)")
   "i" '(consult-imenu :wk "Open imenu"))
  :config
  (require 'consult)
  (require 'consult-imenu)

  (declare-function consult--customize-set "consult"))

(use-package consult-org
  :after (consult org)
  :general
  (:states 'normal :keymaps 'org-mode-map :prefix ef-prefix
   "i" '(consult-org-heading :wk "Search Headings")))

(use-package consult-lsp
  :straight t
  :commands (consult-lsp-symbols))

(provide 'core-selectrum)
