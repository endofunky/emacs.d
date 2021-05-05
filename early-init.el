(defconst ef-emacs-start-time (current-time))

(unless noninteractive
  (add-hook
   'after-init-hook
   #'(lambda ()
       (let ((elapsed (float-time (time-subtract (current-time)
                                                 ef-emacs-start-time))))
         (message "Loading emacs done in %.3fs" elapsed)))))

(defconst ef-initial-file-name-handler-alist file-name-handler-alist
  "Initial value of `file-name-handler-alist' at start-up time.")

(defun ef-reset-startup-values ()
  "Resets early-init.el performance overrides to their initial values"
  (setq-default file-name-handler-alist ef-initial-file-name-handler-alist
                gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))
                gc-cons-percentage (car (get 'gc-cons-percentage 'standard-value))))

(add-hook 'after-init-hook 'ef-reset-startup-values)

(setq-default file-name-handler-alist nil
              gc-cons-threshold most-positive-fixnum
              gc-cons-percentage 0.6
              package--init-file-ensured t
              package-enable-at-startup nil
              package-quickstart t
              package-check-signature nil
              package-archives
              '(("melpa-stable" . "http://stable.melpa.org/packages/")
                ("melpa"        . "http://melpa.org/packages/")
                ("org"          . "http://orgmode.org/elpa/")
                ("gnu"          . "http://elpa.gnu.org/packages/"))
              site-run-file nil)

(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))

(provide 'early-init)
