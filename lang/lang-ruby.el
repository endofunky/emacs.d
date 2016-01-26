(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)
         ("Capfile\\'" . ruby-mode)
         ("\\.cap\\'" . ruby-mode)
         ("\\.thor\\'" . ruby-mode)
         ("\\.rabl\\'" . ruby-mode)
         ("Thorfile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.jbuilder\\'" . ruby-mode)
         (".builder\\'" . ruby-mode)
         ("Podfile\\'" . ruby-mode)
         ("\\.podspec\\'" . ruby-mode)
         ("Puppetfile\\'" . ruby-mode)
         ("Berksfile\\'" . ruby-mode)
         ("Appraisals\\'" . ruby-mode))
  :interpreter ("ruby" . ruby-mode)
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-deep-indent-arglist nil)
  (setq ruby-deep-indent-paren nil)

  (use-package bundler
    :ensure t)

  (use-package ruby-interpolation
    :ensure t
    :diminish ruby-interpolation-mode)

  (use-package yard-mode
    :ensure t
    :diminish yard-mode)

  (use-package rbenv
    :ensure t
    :config
    (global-rbenv-mode 1)
    (setq rbenv-modeline-function 'rbenv--modeline-plain))

  (use-package projectile-rails
    :ensure t
    :commands (projectile-rails-on)
    :init
    (add-hook 'projectile-mode-hook 'projectile-rails-on))

  (use-package ruby-tools
    :ensure t
    :diminish ruby-tools-mode
    :init
    (add-hook 'ruby-mode-hook 'ruby-tools-mode)
    :config
    (evil-define-key 'normal ruby-mode-map ",x\'" 'ruby-tools-to-single-quote-string)
    (evil-define-key 'normal ruby-mode-map ",x\"" 'ruby-tools-to-double-quote-string)
    (evil-define-key 'normal ruby-mode-map ",x:" 'ruby-tools-to-symbol))

  (defun ts/ruby-mode-hook ()
    (yard-mode 1)
    (eldoc-mode 1)
    (global-rbenv-mode 1))

  (add-hook 'ruby-mode-hook 'ts/ruby-mode-hook))

(provide 'lang-ruby)
