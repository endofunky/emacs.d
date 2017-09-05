(use-package uuid
  :ensure t
  :defer t
  :commands (ts/insert-uuid)
  :config
  :init
  (defun ts/insert-uuid ()
    (interactive)
    (require 'uuid)
    (insert (uuid-string))))

(provide 'pkg-uuid)
