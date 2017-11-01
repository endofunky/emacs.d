(use-package uuid
  :ensure t
  :defer t
  :commands (ef-insert-uuid)
  :init
  (defun ef-insert-uuid ()
    (interactive)
    (require 'uuid)
    (insert (uuid-string))))

(provide 'pkg-uuid)
