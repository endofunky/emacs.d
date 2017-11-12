(use-package uuid
  :ensure t
  :defer t
  :commands (ef-insert-uuid)
  :init
  (defun ef-insert-uuid ()
    "Insert a UUID at point."
    (interactive)
    (require 'uuid)
    (insert (uuid-string))))

(provide 'pkg-uuid)
