;;; core-emacs.el --- GnuPG support -*- lexical-binding: t; -*-
(require 'use-package)

(defconst ef-gpg (executable-find "gpg")
  "Path to \"gpg\" executable, if present.")

(use-package epa
  :defer t
  :straight nil
  :when ef-gpg
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
  (poe-popup "*Error*" :size .3 :ephemeral t)

  ;; epa's default key selection window is a bit cumbersome. I rarely select
  ;; more than one key, so override this with a completing read so the key to
  ;; use can be selected using vertico.
  (defun +epa-completion (key)
    "Creates a completion list entry for the given epg-key KEY."
    (list
     (if-let ((primary-user-id (car (epg-key-user-id-list key))))
         (if (stringp (epg-user-id-string primary-user-id))
             (epg-user-id-string primary-user-id)
           (epg-decode-dn (epg-user-id-string primary-user-id)))
       "")
     key))

  (defun +epa-annotate-key (s)
    "Annotate function for epg-key completing read."
    (let ((item (assoc s minibuffer-completion-table)))
      (when-let ((key (cadr item)))
        (let ((primary-sub-key (car (epg-key-sub-key-list key)))
              (validity (epg-sub-key-validity
                         (car (epg-key-sub-key-list key)))))
          (concat
           (propertize " " 'display '(space :align-to 80))
           (epg-sub-key-id primary-sub-key)
           (format " [%s]" (epg-sub-key-validity primary-sub-key)))))))

  (defun +epa-completing-read-key-a (context _prompt &optional names secret)
    "Select GnuPG keys using `completing-read'."
    (when-let ((keys (epg-list-keys context names secret)))
      (let ((completions (mapcar #'+epa-completion keys))
            (completion-extra-properties '(:annotation-function
                                           +epa-annotate-key)))
        (list (cadr (assoc (completing-read "GnuPG key: " completions)
                           completions))))))

  (advice-add 'epa-select-keys :override #'+epa-completing-read-key-a))

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
