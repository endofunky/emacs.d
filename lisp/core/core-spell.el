;;; core-spell.el --- Spell checking -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package flyspell
  :when (executable-find "aspell")
  :custom
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil)
  :hook
  (org-mode . turn-on-flyspell)
  (markdown-mode . turn-on-flyspell)
  (gtm-mode . turn-on-flyspell))

(use-package ispell
  :defer t
  :when (executable-find "aspell")
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
  (ispell-silently-savep t))

(use-package flyspell-correct
  :after flyspell
  :general
  (:keymaps 'flyspell-mode-map
   "C-;" 'flyspell-correct-wrapper))

(provide 'core-spell)
