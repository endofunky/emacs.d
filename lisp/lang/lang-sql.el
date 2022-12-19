;;; lang-sql.el --- SQL configuration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package sql
  :straight nil
  :custom
  ;; PostgreSQL is the most common SQL database these days, so use that as the
  ;; default.
  (sql-product 'postgres)
  :config
  ;; Make double quotes work like a quoting character.
  ;;
  ;; See: https://github.com/Trevoke/sqlup-mode.el/issues/69
  (modify-syntax-entry ?\" "\"" sql-mode-syntax-table))

(use-package sqlup-mode
  :after sql
  :hook
  (sql-mode . sqlup-mode))

(use-package sql-indent
  :after sql
  :hook
  (sql-mode . sqlind-minor-mode))

(provide 'lang-sql)
