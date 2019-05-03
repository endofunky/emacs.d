(setq package-enable-at-startup nil
      package-quickstart t
      package-check-signature nil
      package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa"        . "http://melpa.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")
        ("gnu"          . "http://elpa.gnu.org/packages/")))

(provide 'early-init)
