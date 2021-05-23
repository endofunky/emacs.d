(require 'core-lib)
(require 'cl-extra)
(require 'cl-macs)
(require 'cl-seq)
(require 'subr-x)

(defconst ef-popup-defaults '(:align below :size .4 :popup t :select t)
  "Default values for `shackle-rules' applied to popup buffers created
with `ef-add-popup'.")

(use-package shackle
  :ensure t
  :demand t
  :custom
  (shackle-select-reused-windows nil)
  (shackle-default-alignment 'below)
  (shackle-default-size 0.4)
  (shackle-default-rule '(:same t))
  :functions (ef-shackle
              ef-add-popup)
  :general
  (:states '(normal insert visual motion replace)
   :keymaps 'override
   "M-h" 'ef-popup-cycle-backward
   "M-j" 'ef-popup-demote-buffer
   "M-k" 'ef-popup-promote-buffer
   "M-l" 'ef-popup-cycle-forward
   "M-P" 'ef-popup-switch-popup-buffer
   "M-p" 'ef-popup-toggle)
  :config
  (require 'ef-popup)

  (declare-function ef-popup-mode "ef-popup")
  (ef-popup-mode t)

  (defun ef-shackle (shackle &rest shackles)
    "Adds one or more shackle rules to `shackle-rules'"
    (dolist (rule (cons shackle shackles))
      (add-to-list 'shackle-rules rule)))

  (defun ef-add-popup (mode &rest rules)
    (add-to-list 'shackle-rules
                 (cons mode (ef-plist-merge ef-popup-defaults rules))))

  (ef-add-popup " *Metahelp*" :ephemeral t)
  (ef-add-popup " *undo-tree*" :ephemeral t)
  (ef-add-popup "*Apropos*" :size .3 :ephemeral t)
  (ef-add-popup "*Backtrace*")
  (ef-add-popup "*Checkdoc Status*" :ephemeral t)
  (ef-add-popup "*Command History*")
  (ef-add-popup "*Help*" :ephemeral t)
  (ef-add-popup "*Messages*")
  (ef-add-popup "*Occur*" :ephemeral t)
  (ef-add-popup "*Pp Eval Output*")
  (ef-add-popup "*Warnings*" :ephemeral t)
  (ef-add-popup "*company-documentation*" :ephemeral t)
  (ef-add-popup "*compilation*")
  (ef-add-popup "\\`\\*WoMan.*?\\*\\'" :regexp t :ephemeral t)
  (ef-add-popup "\\`\\*info.*?\\*\\'" :regexp t :ephemeral t :size 0.5)
  (ef-add-popup 'Info-mode :ephemeral t :size 0.5)
  (ef-add-popup 'comint-mode)
  (ef-add-popup 'compilation-mode)
  (ef-add-popup 'info-mode :ephemeral t :size 0.5)

  (shackle-mode t))

(provide 'core-shackle)
