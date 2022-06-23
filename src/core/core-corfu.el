(require 'core-lib)

(use-package corfu
  :straight (corfu :type git
                   :host github
                   :repo "minad/corfu"
                   :branch "main"
                   :files ("*.el" "extensions/*.el"))
  :custom
  (corfu-cycle t)
  (corfu-echo-documentation nil)
  (corfu-min-width 24)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  (corfu-bar-width 1.0)
  (corfu-right-margin-width 2.0)
  (corfu-left-margin-width 1.0)
  :commands (corfu-mode)
  :general
  (:keymaps 'corfu-map
   "<escape>" 'corfu-quit
   "C-n" 'corfu-next
   "C-p" 'corfu-previous
   "M-m" 'ef-corfu-move-to-minibuffer)
  :hook
  (after-init . global-corfu-mode)
  (after-init . corfu-history-mode)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history)

  (defun ef-corfu-move-to-minibuffer ()
    "Move current region completion to minibuffer via consult."
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  ;; Ensure corfu keybinds correctly work together with evil-mode
  ;;
  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map))

(use-package cape
  :after corfu
  :demand t
  :functions (ef-capf-merge-super-capf
              ef-capf-restore-super-capf)
  :commands (cape-dabbrev)
  :custom
  (cape-dabbrev-check-other-buffers nil)
  :general
  ;; Vim-style insert-state C-x keybinds for completions.
  (:state 'insert :prefix "C-x"
   "C-f" 'cape-file
   "C-k" 'cape-dict
   "C-n" 'cape-keyword
   "C-s" 'dabbrev-completion
   "s" 'cape-ispell)
  :config
  (declare-function cape-super-capf "cape")

  ;; Append in order to prefer the mode-specific `completion-at-point-functions'
  ;; since they provide documentation and `cape-dabbrev' does not.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)

  ;; `completion-at-point-functions' are run one by one. As soon as one returns
  ;; results the chain is stopped, eg. results from multiple functions aren't
  ;; merged. Instead, we merge all the functions in a `cape-super-capf' before
  ;; completion and restore the original value when completion has finished.
  (defvar ef-capf-pre-merge nil
    "Placeholder variable to store the original value of buffer-local value of
`completion-at-point-functions'.")

  (defun ef-capf-merge-super-capf (&rest _)
    "Advice to merge all `completion-at-point-functions' in a `cape-super-capf'
to use during completion."
    (unless ef-capf-pre-merge
      (setq-local ef-capf-pre-merge completion-at-point-functions)
      (setq-local completion-at-point-functions
                  (list (apply #'cape-super-capf
                               (remove t completion-at-point-functions))))))

  (defun ef-capf-restore-super-capf (&rest _)
    "Restores the previous `completion-at-point-functions' after completion
has finished."
    (when ef-capf-pre-merge
      (setq-local completion-at-point-functions ef-capf-pre-merge)
      (setq-local ef-capf-pre-merge nil)))

  ;; Adding advices around `corfu--setup' or `corfu--update' won't work here,
  ;; but this does *shrug*.
  (advice-add 'completion-at-point :before #'ef-capf-merge-super-capf)
  ;; This one is needed to work correctly when `corfu-auto' is true.
  (advice-add 'corfu--capf-wrapper :before #'ef-capf-merge-super-capf)
  (advice-add 'corfu--teardown :after #'ef-capf-restore-super-capf))

(use-package corfu-doc
  :after corfu
  :custom
  (corfu-doc-auto nil)
  (corfu-doc-max-width 90)
  (corfu-doc-max-height 24)
  :general
  (:keymaps 'corfu-map
   [remap corfu-show-documentation] 'corfu-doc-toggle
   "C-i" 'corfu-doc-toggle
   "C-M-p" 'corfu-doc-scroll-down
   "C-M-n" 'corfu-doc-scroll-up)
  :hook
  (corfu-mode . corfu-doc-mode))

(use-package emacs
  :after corfu
  :straight nil
  :custom
  (completion-cycle-threshold 2)
  ;; Enable indentation+completion using the TAB key.
  ;; Completion is often bound to M-TAB.
  (tab-always-indent 'complete))

(use-package popon
  :defer t
  :straight (popon
             :type git
             :repo "https://codeberg.org/akib/emacs-popon.git"))

(use-package corfu-terminal
  :when (not window-system)
  :defer t
  :straight (corfu-terminal
             :type git
             :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :hook
  (after-init . corfu-terminal-mode))

(use-package corfu-doc-terminal
  :when (not window-system)
  :defer t
  :straight (corfu-doc-terminal
             :type git
             :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
  :hook
  (after-init . corfu-doc-terminal-mode))

(provide 'core-corfu)
