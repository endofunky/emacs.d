;;; core-nix.el --- Nix/NixOS integration -*- lexical-binding: t; -*-
(require 'use-package)

(defconst +nix--profile-paths
  (reverse (split-string (or (getenv "NIX_PROFILES") ""))))

(when (featurep 'comp)
  ;; Append native-comp subdirectories from `NIX_PROFILES'.
  (setq native-comp-eln-load-path
        (cl-remove-duplicates
         (append (mapcar (lambda (profile-dir)
                           (concat profile-dir "/share/emacs/native-lisp/"))
                         +nix--profile-paths)
                 native-comp-eln-load-path))))

(use-package find-func
  :straight nil
  :defer t
  :config
  ;; Set correct `source-directory' on Nix/NixOS.
  ;;
  ;; We don't enable the global site-lisp files, so we have to do this
  ;; ourselves.
  (unless (string-prefix-p "/nix/store" source-directory)
    (if-let* ((path (executable-find "emacs"))
              ;; Expand symlink
              (path (file-truename path))
              ;; Check if using emacs from Nix
              ((string-prefix-p "/nix/store" path))
              ;; Remove binary dir
              (path (string-remove-suffix "/bin/emacs" path))
              ;; Add: /share/emacs/29.0.50/src
              (path (expand-file-name "share" path))
              (path (expand-file-name "emacs" path))
              (path (expand-file-name emacs-version path))
              (path (expand-file-name "src" path)))
        (when (file-exists-p path)
          (setq find-function-C-source-directory path)))))

(use-package tramp-sh
  :straight nil
  :defer t
  :config
  ;; Make tramp work for remote NixOS machines.
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))

(provide 'core-nix)
