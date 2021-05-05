(defun ef-init-debug-p ()
  "Returns true if the EMACS_INIT_DEBUG environment variable is set."
  (stringp (getenv "EMACS_INIT_DEBUG")))

;; Less warnings unless we're in debug mode.
(when (ef-init-debug-p)
  (setq-default warning-minimum-level :error))

(setq-default
 ;; Increase *Messages* buffer size
 message-log-max 16384
 ;; Silence ad-handle-definition about advised functions getting redefined.
 ad-redefinition-action 'accept
 ;; Less byte-compiler noise
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
 byte-compile-verbose (ef-init-debug-p))

(let ((default-directory (expand-file-name "elpa" user-emacs-directory)))
  (unless (file-exists-p default-directory)
    (make-directory default-directory))
  (normal-top-level-add-subdirs-to-load-path))

(unless (fboundp 'package-installed-p)
  (package-initialize t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(customize-set-variable 'use-package-enable-imenu-support t)
(require 'use-package)

(if (ef-init-debug-p)
    (setq-default use-package-verbose t))

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

(dolist (path '("core" "base" "lang" "private"))
  (let ((path (expand-file-name path user-emacs-directory)))
    (add-to-list 'load-path path)
    (dolist (file (directory-files path nil "\\.el$"))
      (require (intern (file-name-sans-extension file)) nil t))))

(provide 'init)
