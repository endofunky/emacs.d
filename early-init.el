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

;; Disable graphical elements here to avoid flickering during startup.
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
  (scroll-bar-mode -1))

(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))

(unless (memq (window-system) '(mac ns))
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1)))

(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))

(provide 'early-init)
