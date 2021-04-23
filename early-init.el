(defconst ef-emacs-start-time (current-time))

(defconst ef-initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")

(defconst ef-initial-gc-cons-percentage gc-cons-percentage
  "Initial value of `gc-cons-percentage' at start-up time.")

(defconst ef-initial-file-name-handler-alist file-name-handler-alist
  "Initial value of `file-name-handler-alist' at start-up time.")

(defun ef-reset-startup-values ()
  "Resets `gc-cons-threshold` to it's initial value"
  (setq-default file-name-handler-alist ef-initial-file-name-handler-alist
                gc-cons-threshold ef-initial-gc-cons-threshold
                gc-cons-percentage ef-initial-gc-cons-percentage))

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
