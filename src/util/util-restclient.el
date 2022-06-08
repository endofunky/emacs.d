(require 'core-shackle)

(defconst ef-restclient-buffer-name "*restclient*"
  "Scratch buffer name for restclient-mode.")

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode))
  :commands (ef-restclient
             restclient-mode)
  :straight t
  :general
  (:states 'normal :prefix ef-prefix
   "R" '(ef-restclient :wk "Toggle RESTclient"))
  (:states 'normal :prefix ef-prefix :keymaps 'restclient-mode-map
   "c" '(nil :wk "RESTclient Menu")
   "cc" '(restclient-http-send-current :wk "Send Request")
   "cr" '(restclient-http-send-current-raw :wk "Send Request (Raw)")
   "cn" '(restclient-jump-next :wk "Jump to Next Request")
   "cp" '(restclient-jump-prev :wk "Jump to Previous Request")
   "c." '(restclient-mark-current :wk "Mark Request")
   "cu" '(restclient-copy-curl-command :wk "Copy cURL Command")
   "cn" '(restclient-narrow-to-current :wk "Narrow Region to Request")
   "ci" '(restclient-show-info :wk "Show Info"))
  :config
  (ef-add-popup "*HTTP Response*")
  (ef-add-popup "*Restclient Info*" :ephemeral t)
  (defun ef-restclient ()
    "Toggle scratch buffer for `restclient-mode'."
    (interactive)
    (if (string= ef-restclient-buffer-name
                 (buffer-name (current-buffer)))
        (bury-buffer)
      (unless (get-buffer ef-restclient-buffer-name)
        (with-current-buffer (generate-new-buffer ef-restclient-buffer-name)
          (set-buffer-modified-p nil)
          (restclient-mode)))
      (switch-to-buffer ef-restclient-buffer-name))))

(use-package ob-restclient
  :after (restclient org)
  :straight t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package company-restclient
  :straight t
  :after (restclient company)
  :config
  (add-to-list 'company-backends 'company-restclient))

(provide 'util-restclient)
