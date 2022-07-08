;;; core-emacs.el --- GnuPG support -*- lexical-binding: t; -*-
(require 'use-package)

(defconst ef-gpg (executable-find "gpg")
  "Path to \"gpg\" executable, if present.")

(use-package epg-config
  :straight nil
  :when ef-gpg
  :defer t
  :custom
  (epg-pinentry-mode 'loopback))

(use-package pinentry
  :when ef-gpg
  :hook (after-init . pinentry-start))

(provide 'core-gpg)
