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
  (setq ruby-deep-arglist nil)
  (setq ruby-deep-indent-paren nil)

  (ts/define-repl ts/repl-ruby "*ruby*" 'inf-ruby)
  (evil-define-key 'normal ruby-mode-map ",r" 'ts/repl-ruby)
  (evil-define-key 'normal inf-ruby-mode-map ",r" 'ts/repl-ruby)
  (evil-define-key 'normal ruby-mode-map ",eb" 'ruby-send-buffer)
  (evil-define-key 'visual ruby-mode-map ",er" 'ruby-send-region)

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

(use-package bundler
  :after ruby-mode
  :ensure t)

(use-package ruby-interpolation
  :after ruby-mode
  :ensure t
  :diminish ruby-interpolation-mode)

(use-package yard-mode
  :after ruby-mode
  :ensure t
  :diminish yard-mode)

(use-package rbenv
  :after ruby-mode
  :ensure t
  :config
  (global-rbenv-mode 1)
  (setq rbenv-modeline-function 'rbenv--modeline-plain))

(use-package ruby-test-mode
  :after ruby-mode
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'ruby-test-mode)
  (evil-define-key 'normal ruby-test-mode-map ",t" 'ruby-test-run))

(use-package projectile-rails
  :after ruby-mode
  :diminish projectile-rails-mode
  :ensure t
  :init
  (ts/define-repl ts/repl-projectile-rails-console "*rails*" #'(lambda () (projectile-rails-console nil)))
  :config
  (projectile-rails-global-mode t)
  (evil-define-key 'normal projectile-rails-mode-map ",r" 'ts/repl-projectile-rails-console))

(use-package ruby-tools
  :after ruby-mode
  :ensure t
  :diminish ruby-tools-mode
  :init
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  :config
  (evil-define-key 'normal ruby-mode-map ",x:" 'ruby-tools-to-symbol)
  (evil-define-key 'normal ruby-mode-map ",x\"" 'ruby-tools-to-double-quote-string)
  (evil-define-key 'normal ruby-mode-map ",x\'" 'ruby-tools-to-single-quote-string))

(provide 'lang-ruby)
