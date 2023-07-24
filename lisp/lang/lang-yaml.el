;;; lang-yaml.el --- YAML configuration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package yaml-mode
  :defer t
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("\\.ya?ml.dist\\'" . yaml-mode)
         ("\\.ya?ml.erb\\'" . yaml-mode))
  :config
  (+add-hook yaml-mode-hook
    ;; yaml-mode derives from text-mode and we enable flyspell-mode for
    ;; text-mode in core-spell, so we disable it here again.
    (flyspell-mode -1)))

(provide 'lang-yaml)
