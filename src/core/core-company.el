(require 'core-direnv)
(require 'core-evil)

(use-package company
  :demand t
  :custom
  ;; These auto-complete the current selection when
  ;; `company-auto-commit-chars' is typed. This is too magical. We
  ;; already have the much more explicit RET and TAB.
  (company-auto-commit nil)
  (company-auto-complete #'ef-company-visible-and-explicit-action-p)
  (company-begin-commands '(self-insert-command))
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-other-buffers nil)
  (company-frontends '(company-pseudo-tooltip-frontend))
  (company-global-modes '(not message-mode help-mode))
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 12)
  (company-tooltip-margin 2)
  (evil-complete-next-func 'ef-evil-complete-lambda)
  (evil-complete-previous-func 'ef-evil-complete-lambda)
  :general
  (:states 'insert :keymaps 'prog-mode-map
   "<tab>" 'ef-tab-indent-or-complete)
  :functions (ef-check-expansion
	      ef-minibuffer-completion-hook)
  :hook
  (company-mode . evil-normalize-keymaps)
  :config
  (customize-set-variable
   'company-backend
   (delete 'company-clang company-backends))

  (declare-function company-tooltip-visible-p "company")
  (declare-function company-complete-common "company")
  (declare-function company-explicit-action-p "company")
  (declare-function company-complete "company")
  (declare-function global-company-mode "company")

  (defun ef-company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))

  (defun ef-check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun ef-tab-indent-or-complete ()
    "Either indent or start company completion."
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (ef-check-expansion)
          (company-complete-common)
        (indent-for-tab-command))))

  (defun ef-evil-complete-lambda (arg)
    "Ignores passed in arg like a lambda and runs company-complete"
    (company-complete))

  (global-company-mode t))

(use-package company-statistics
  :after company
  :hook
  (company-mode . company-statistics-mode))

(use-package company-box
  :if window-system
  :after company
  :hook (company-mode . company-box-mode)
  :defines (company-box-doc-enable
            company-box-doc-frame)
  :functions (ef-company-box--get-frame-ad
              ef-company-box-doc-ad)
  :custom
  (company-box-show-single-candidate t)
  (company-box-backends-colors nil)
  (company-box-max-candidates 50)
  :config
  (declare-function frame-local-get "frame-local")
  (declare-function frame-local-set "frame-local")

  ;; More elaborate frame-live-p checks from doom emacs.
  (defun ef-company-box--get-frame-ad (frame)
    (if (frame-live-p frame) frame))

  (advice-add 'company-box--get-frame
              :filter-return #'ef-company-box--get-frame-ad)

  (defun ef-company-box-doc-ad (orig-fun frame)
    (and company-box-doc-enable
         (frame-local-getq company-box-doc-frame frame)
         (not (frame-live-p (frame-local-getq company-box-doc-frame frame)))
         (frame-local-setq company-box-doc-frame nil frame)))

  (advice-add 'company-box-doc :before #'ef-company-box-doc-ad))

(provide 'core-company)
