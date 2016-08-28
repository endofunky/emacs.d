(use-package ruby-mode
  :mode (("Appraisals\\'" . ruby-mode)
         ("Berksfile\\'" . ruby-mode)
         ("Brewfile\\'" . ruby-mode)
         ("Capfile\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)
         ("Podfile\\'" . ruby-mode)
         ("Puppetfile\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("Thorfile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.builder\\'" . ruby-mode)
         ("\\.cap\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.jbuilder\\'" . ruby-mode)
         ("\\.podspec\\'" . ruby-mode)
         ("\\.rabl\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.rb\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.thor\\'" . ruby-mode))
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
    :diminish projectile-rails-mode
    :ensure t
    :commands (projectile-rails-on)
    :init
    (add-hook 'projectile-mode-hook 'projectile-rails-on)
    :config
    (evil-define-key 'normal projectile-rails-mode-map ",r," 'projectile-rails-goto-file-at-point)
    (evil-define-key 'normal projectile-rails-mode-map ",rc" 'projectile-rails-find-current-controller)
    (evil-define-key 'normal projectile-rails-mode-map ",rf" 'projectile-rails-find-current-fixture)
    (evil-define-key 'normal projectile-rails-mode-map ",rh" 'projectile-rails-find-current-helper)
    (evil-define-key 'normal projectile-rails-mode-map ",rm" 'projectile-rails-find-current-model)
    (evil-define-key 'normal projectile-rails-mode-map ",rs" 'projectile-rails-find-current-spec)
    (evil-define-key 'normal projectile-rails-mode-map ",rt" 'projectile-rails-find-current-test)
    (evil-define-key 'normal projectile-rails-mode-map ",rv" 'projectile-rails-find-current-view))

  (use-package ruby-tools
    :ensure t
    :diminish ruby-tools-mode
    :init
    (add-hook 'ruby-mode-hook 'ruby-tools-mode)
    :config
    (evil-define-key 'normal ruby-mode-map ",x:" 'ruby-tools-to-symbol)
    (evil-define-key 'normal ruby-mode-map ",x\"" 'ruby-tools-to-double-quote-string)
    (evil-define-key 'normal ruby-mode-map ",x\'" 'ruby-tools-to-single-quote-string))

  (defun ts/ruby-mode-hook ()
    (yard-mode 1)
    (eldoc-mode 1)
    (global-rbenv-mode 1)

    (require 'smartparens-ruby)
    (sp-with-modes '(ruby-mode)
      (sp-local-pair "[" nil :post-handlers '((ts/sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "{" nil :post-handlers '((ts/sp-create-newline-and-enter-sexp "RET")))
      (sp-local-pair "(" nil :post-handlers '((ts/sp-create-newline-and-enter-sexp "RET")))))

  (add-hook 'ruby-mode-hook 'ts/ruby-mode-hook))

(provide 'lang-ruby)
