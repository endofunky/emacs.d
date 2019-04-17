(use-package editorconfig
  :ensure t
  ;; :diminish editorconfig-mode
  :commands editorconfig-mode
  :mode ("\\.editorconfig\\'" . conf-unix-mode)
  :preface
  (defconst ef-editorconfig-location
    (locate-file "editorconfig" exec-path))

  (unless ef-editorconfig-location
    (warn "editorconfig executable missing from PATH. editorconfig-mode will not be enabled"))

  :custom
  (editorconfig-exec-path ef-editorconfig-location)
  :if ef-editorconfig-location
  :init
  (defun ef-editorconfig-mode ()
    (editorconfig-mode 1)
    (editorconfig-apply))
  :hook (find-file . ef-editorconfig-mode))

(provide 'pkg-editorconfig)
