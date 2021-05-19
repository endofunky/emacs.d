(require 'core-evil)

(use-package company
  :ensure t
  :demand t
  :custom
  (company-auto-complete #'ef-company-visible-and-explicit-action-p)
  (company-begin-commands '(self-insert-command))
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-frontends '(company-pseudo-tooltip-frontend))
  (company-global-modes '(not message-mode help-mode))
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 2)
  (company-require-match 'never)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 12)
  (company-tooltip-margin 2)
  (evil-complete-next-func 'ef-evil-complete-lambda)
  (evil-complete-previous-func 'ef-evil-complete-lambda)
  :general
  (:states 'normal :keymaps 'company-active-map
	   "<return>" 'company-complete-selection
	   "<tab>" 'company-select-next-if-tooltip-visible-or-complete-selection
	   "C-f" 'company-complete-selection
	   "C-n" #'company-select-next-or-abort
	   "C-p" #'company-select-previous-or-abort)
  (:states 'insert
	   "<tab>" 'ef-tab-indent-or-complete)
  :functions (ef-check-expansion
	      ef-minibuffer-completion-hook)
  :config
  (declare-function company-tooltip-visible-p "company")
  (declare-function company-complete-common "company")
  (declare-function company-explicit-action-p "company")

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
  :ensure t
  :after company
  :hook
  (company-mode . company-statistics-mode))

(use-package company-flx
  :ensure t
  :after company
  :custom
  (company-flx-limit 750)
  :hook
  (company-mode . company-flx-mode))

(use-package company-box
  :ensure t
  :after company
  :custom
  (company-box-backends-colors nil)
  (company-box-scrollbar nil)
  :hook
  (company-mode . company-box-mode))

(provide 'core-company)
