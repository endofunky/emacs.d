(defun ts/js-mode-hook ()
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'ts/js-mode-hook)

(provide 'lang-js)
