(defun ef-js-mode-hook ()
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'ef-js-mode-hook)

(provide 'lang-js)
