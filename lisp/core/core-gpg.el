;;; core-emacs.el --- GnuPG support -*- lexical-binding: t; -*-
(require 'use-package)

(defconst ef-gpg (executable-find "gpg")
  "Path to \"gpg\" executable, if present.")

(use-package epa
  :defer t
  :straight nil
  :custom
  ;; Display status information in echo area instead of a new window.
  (epa-popup-info-window nil)
  :general
  (:states 'normal :prefix ef-leader
   "G"   '(nil :wk "GnuPG")
   "Gd"  '(nil :wk "Decrypt")
   "Gdf" '(epa-decrypt-file :wk "File")
   "Ge"  '(nil :wk "Encrypt")
   "Gef" '(epa-encrypt-file :wk "File")
   "Gi"  '(epa-insert-keys :wk "Insert keys")
   "GI"  '(nil :wk "Import")
   "GIf" '(epa-import-keys :wk "File")
   "Gk"  '(epa-list-keys :wk "List keys")
   "GK"  '(epa-list-secret-keys :wk "List secret keys")
   "Gs"  '(nil :wk "Sign")
   "Gsf" '(epa-sign-file :wk "File")
   "Gv"  '(nil :wk "Verify")
   "Gvf" '(epa-verify-file :wk "File"))
  (:states 'visual :prefix ef-leader
   "Gdr" '(epa-decrypt-region :wk "Region")
   "Ger" '(epa-encrypt-region :wk "Region")
   "GIr" '(epa-import-keys-region :wk "Region")
   "Gsr" '(epa-sign-region :wk "Region")
   "Gvr" '(epa-verify-region :wk "Region"))
  :config
  (poe-popup 'epa-key-list-mode :select t :size .4 :shrink t :ephemeral t)
  (poe-popup "*Error*" :size .3 :ephemeral t))

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
