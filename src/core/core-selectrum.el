(use-package selectrum
  :ensure t
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
  :ensure t
  :custom
  (selectrum-extend-current-candidate-highlight t)
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (orderless-matching-styles '(orderless-regexp
                               orderless-flex))
  (completion-styles '(orderless)))

(use-package marginalia
  :after selectrum
  :ensure t
  :demand t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light))
  :config
  (marginalia-mode t))

(use-package consult
  :ensure t
  :demand t
  :custom
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

(use-package embark
  :after selectrum
  :ensure t
  :general
  (:keymaps 'minibuffer-mode-map
   "C-." 'embark-act)
  :custom
  (embark-action-indicator (lambda (map target)
                             (which-key--show-keymap "Embark" map nil nil
                                                     'no-paging)
                             #'which-key--hide-popup-ignore-command)
                           embark-become-indicator embark-action-indicator)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(provide 'core-selectrum)
