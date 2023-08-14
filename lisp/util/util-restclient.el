;;; util-restclient-.el --- restclient integration -*- lexical-binding: t; -*-
(require 'core-lib)
(require 'core-popup)

(defconst ef-restclient-buffer-name "*restclient*"
  "Scratch buffer name for restclient-mode.")

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode))
  :commands (+restclient restclient-mode)
  :defines (restclient-info-buffer-name
            restclient-same-buffer-response-name)
  :general
  (:states 'normal :prefix ef-leader
   "R" '(+restclient :wk "Toggle RESTclient"))
  (:states 'normal :prefix ef-leader :keymaps 'restclient-mode-map
   "c" '(nil :wk "restclient")
   "cc" '(restclient-http-send-current :wk "Send request")
   "cr" '(restclient-http-send-current-raw :wk "Send request (raw)")
   "cn" '(restclient-jump-next :wk "Jump to next request")
   "cp" '(restclient-jump-prev :wk "Jump to previous request")
   "c." '(restclient-mark-current :wk "Mark request")
   "cu" '(restclient-copy-curl-command :wk "Copy cURL command")
   "cn" '(restclient-narrow-to-current :wk "Narrow region to request")
   "ci" '(restclient-show-info :wk "Show info"))
  :config
  (poe-popup restclient-same-buffer-response-name :size 0.4)
  (poe-popup restclient-info-buffer-name :ephemeral t)

  (defun +restclient ()
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

(use-package restclient-jq
  :after restclient)

(use-package ob-restclient
  :after (restclient org)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(provide 'util-restclient)
