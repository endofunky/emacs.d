(require 'core-lib)

(use-package vertico
  :straight (vertico :type git
                     :host github
                     :repo "minad/vertico"
                     :branch "main"
                     :files ("*.el" "extensions/*.el"))
  :custom
  (vertico-count 20)
  (vertico-cycle t)
  (vertico-scroll-margin 0)
  :hook
  (minibuffer-setup . vertico-repeat-save)
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode))

(use-package orderless
  :after vertico
  :straight t
  :custom
  (orderless-style-dispatchers '(ef-orderless-without-if-bang
                                 ef-orderless-with-if-equals))
  (orderless-matching-styles '(orderless-regexp
                               orderless-flex))
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (comp)
  :config
  (defun ef-orderless-with-if-equals (pattern index total)
    "Orderless style dispatcher to literal match results using equal sign."
    (when (string-prefix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 1))))

  (defun ef-orderless-without-if-bang (pattern index total)
    "Orderless style dispatcher to discard results matching literal following
exclamation mark."
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

(use-package marginalia
  :after vertico
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
  ([remap apropos] 'consult-apropos)
  ([remap bookmark-jump] 'consult-bookmark)
  ([remap evil-show-marks] 'consult-mark)
  ([remap evil-show-registers] 'consult-register)
  ([remap goto-line] 'consult-goto-line)
  ([remap imenu] 'consult-imenu)
  ([remap locate] 'consult-locate)
  ([remap load-theme] 'consult-theme)
  ([remap man] 'consult-man)
  ([remap recentf-open-files] 'consult-recent-file)
  ([remap switch-to-buffer] 'consult-buffer)
  ([remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  ([remap yank-pop] 'consult-yank-pop)
  (:states 'normal :prefix ef-prefix
   "/" '(consult-ripgrep :wk "Grep (rg)")
   "i" '(consult-imenu :wk "Open imenu"))
  :config
  (require 'consult)
  (require 'consult-imenu)

  (advice-add #'multi-occur :override #'consult-multi-occur)
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple))

(use-package consult-org
  :after (consult org)
  :general
  (:states 'normal :keymaps 'org-mode-map :prefix ef-prefix
   "i" '(consult-org-heading :wk "Search Headings")))

(use-package consult-lsp
  :straight t
  :commands (consult-lsp-symbols))

(provide 'core-vertico)
