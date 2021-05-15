(use-package flyspell
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode)))

(use-package ispell
  :hook (text-mode . ispell-minor-mode))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :general
  (:keymaps 'flyspell-mode-map
	    "C-;" 'flyspell-correct-wrapper))

(provide 'core-spell)
