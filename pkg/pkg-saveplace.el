(use-package saveplace
  :config
  (require 'saveplace)
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (setq-default save-place t))

(provide 'pkg-saveplace)
