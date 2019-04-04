(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :mode ("\\.editorconfig\\'" . conf-unix-mode)
  :preface
  (defconst ef-editorconfig-location
    (locate-file "editorconfig" exec-path))

  (unless ef-editorconfig-location
    (warn "editorconfig executable missing from PATH. editorconfig-mode will not be enabled"))

  :if ef-editorconfig-location
  :config
  (editorconfig-mode 1))

(provide 'pkg-editorconfig)