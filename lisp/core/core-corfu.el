;;; core-corfu.el --- In-region completion -*- lexical-binding: t; -*-
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
   "M-m" '+corfu-move-to-minibuffer)
  :hook
  (ef-first-command . global-corfu-mode)
  (ef-first-command . corfu-history-mode)
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history))

  (remove-hook 'completion-at-point-functions
               #'tags-completion-at-point-function)

  (defun +corfu-move-to-minibuffer ()
    "Move current region completion to minibuffer via consult."
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  ;; Ensure corfu keybinds correctly work together with evil-mode
  ;;
  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  (advice-add 'corfu--setup :after #'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after #'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map))

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

(provide 'core-corfu)
