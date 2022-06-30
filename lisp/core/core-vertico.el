;;; core-vertico.el --- Minibuffer completion -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'core-shackle)

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
  ;; Register `vertico-repeat'.
  (minibuffer-setup . vertico-repeat-save)
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :general
  (:keymaps 'vertico-map
   "C-SPC" #'ef-vertico-restrict-to-matches)
  :hook (ef-first-command . vertico-mode)
  :config
  (defun ef-vertico-restrict-to-matches ()
    (interactive)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert " ")
      (add-text-properties (minibuffer-prompt-end) (point-max)
                           '(invisible t read-only t cursor-intangible t rear-nonsticky t)))))

(use-package orderless
  :after vertico
  :custom
  (orderless-style-dispatchers '(ef-orderless-without-if-bang
                                 ef-orderless-with-if-equals))
  (orderless-matching-styles '(orderless-regexp
                               orderless-flex))
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  :commands (orderless-filter)
  :config
  (defun ef-orderless-with-if-equals (pattern _index _total)
    "Orderless style dispatcher to literal match results using equal sign."
    (when (string-prefix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 1))))

  (defun ef-orderless-without-if-bang (pattern _index _total)
    "Orderless style dispatcher to discard results matching literal following
exclamation mark."
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

(use-package fussy
  :after vertico
  :ensure t
  :straight (fussy :type git
                   :host github
                   :repo "jojojames/fussy")
  :custom
  (fussy-filter-fn 'fussy-filter-orderless-flex)
  (fussy-score-fn 'fussy-fzf-native-score)
  ;; For example, project-find-file uses 'project-files which uses
  ;; substring completion by default. Set to nil to make sure it's using
  ;; fussy.
  (completion-category-defaults nil)
  (completion-category-overrides nil)
  :config
  (push 'fussy completion-styles)

  ;; `eglot' defaults to flex, so set an override to point to flx instead.
  (with-eval-after-load 'eglot
    (add-to-list 'completion-category-overrides
                 '(eglot (styles fussy basic)))))

(use-package fzf-native
  :after vertico
  :straight (:repo "dangduc/fzf-native"
             :host github
             :files (:defaults "*.c" "*.h" "*.txt"))
  :custom
  ;; Don't ask if we want to compile the module.
  (fzf-native-always-compile-module t)
  :config
  (ef-add-popup fzf-native-module-install-buffer-name :ephemeral t)
  ;; As opposed to the included binary, fzf-native will build the module
  ;; using -march=native if we do it ourselves so it creates optimal code
  ;; for the used CPU.
  (fzf-native-load-own-build-dyn))

(use-package marginalia
  :after vertico
  :demand t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light))
  :defines (marginalia-command-categories)
  :commands (marginalia-mode)
  :config
  (marginalia-mode t))

(use-package consult
  :defer t
  :custom
  (consult-preview-key nil)
  (consult-project-root-function #'ef-project-root)
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
  (:states 'normal :prefix ef-leader
   "/" '(consult-ripgrep :wk "Grep (rg)")
   "i" '(consult-imenu :wk "Open imenu"))
  :functions (consult-multi-occur
              consult-completing-read-multiple)
  :config
  (require 'consult)
  (require 'consult-imenu))

(use-package consult-org
  :after (consult org)
  :straight nil
  :general
  (:states 'normal :keymaps 'org-mode-map :prefix ef-leader
   "i" '(consult-org-heading :wk "Search Headings")))

(use-package consult-xref
  :straight nil
  :defer t
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package embark
  :after vertico
  :general
  (:keymaps 'minibuffer-mode-map
   "C-." 'embark-act)
  :custom
  (embark-indicators '(ef-embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :functions (ef-embark-hide-which-key-indicator
              ef-embark-which-key-indicator)
  :defines (embark-indicators)
  :config
  (declare-function embark-completing-read-prompter "embark")
  (declare-function embark--truncate-target "embark")

  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  (defun ef-embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key. The which-key
help message will show the type and value of the current target followed by an
ellipsis if there are further targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (defun ef-embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read
prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'ef-embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'ef-embark-hide-which-key-indicator)

  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'core-vertico)
