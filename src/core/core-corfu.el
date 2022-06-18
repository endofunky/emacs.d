(require 'core-lib)

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.01)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-echo-documentation nil)
  (corfu-min-width 24)
  (corfu-preview-current nil)
  :commands (corfu-mode)
  :general
  (:keymaps 'corfu-map
   "<escape>" 'corfu-quit)
  :hook
  (after-init . global-corfu-mode)
  (minibuffer-setup . ef-corfu-enable-always-in-minibuffer)
  :config
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun ef-corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1))))

(use-package corfu-doc
  :after corfu
  :custom
  (corfu-doc-auto nil)
  (corfu-doc-max-width 90)
  (corfu-doc-max-height 24)
  :general
  (:keymaps 'corfu-map
   [remap corfu-show-documentation] 'corfu-doc-toggle
   "C-h" 'corfu-doc-toggle
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
