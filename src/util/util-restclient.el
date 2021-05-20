(require 'core-shackle)

(defconst ef-restclient-buffer-name "*restclient*"
  "Scratch buffer name for restclient-mode.")

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode))
  :commands (ef-restclient
             restclient-mode)
  :ensure t
  :general
  (:states 'normal :prefix ef-prefix
	   "R" '(ef-restclient :wk "Toggle RESTclient"))
  :config
  (ef-add-popup "*HTTP Response*")
  (defun ef-restclient ()
    "Toggle scratch buffer for `restclient-mode'."
    (interactive)
    (if (string= ef-restclient-buffer-name
                 (buffer-name (current-buffer)))
        (bury-buffer)
      (unless (get-buffer ef-restclient-buffer-name)
        (with-current-buffer (generate-new-buffer ef-restclient-buffer-name)
          (set-buffer-modified-p nil)
          (restclient-mode t)))
      (switch-to-buffer ef-restclient-buffer-name))))

(use-package ob-restclient
  :after (restclient org)
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package company-restclient
  :ensure t
  :after (restclient company)
  :config
  (add-to-list 'company-backends 'company-restclient))

(provide 'util-restclient)
