;;; lang-sql.el --- SQL configuration -*- lexical-binding: t; -*-
(require 'core-lib)

(use-package sql
  :straight nil
  :custom
  ;; PostgreSQL is the most common SQL database these days, so use that as the
  ;; default.
  (sql-postgres-login-params nil)
  (sql-connection-alist
   '((psql-docker (sql-product 'postgres)
                  (sql-database "postgres://postgres:postgres@localhost:5432/"))
     (psql-vagrant (sql-product 'postgres)
                   (sql-database "postgres://vagrant:vagrant@localhost:5432/"))))
  :general
  (:prefix ef-local-leader :states 'normal :keymaps 'sql-mode-map
   "e"   '(nil :wk "Eval")
   "eb"  '(sql-send-buffer :wk "Buffer")
   "ee"  '(sql-send-paragraph :wk "Paragraph")
   "r"   '(nil :wk "REPL")
   "rr"  '(sql-connect :wk "SQL Connect"))
  (:prefix ef-local-leader :states 'visual :keymaps 'sql-mode-map
   "e"   '(nil :wk "Eval")
   "er"  '(sql-send-region :wk "Region"))
  :config
  (poe-popup 'sql-interactive-mode :size 0.4 :select t)
  ;; Make double quotes work like a quoting character.
  ;;
  ;; See: https://github.com/Trevoke/sqlup-mode.el/issues/69
  (modify-syntax-entry ?\" "\"" sql-mode-syntax-table))

(use-package sqlup-mode
  :after sql
  :custom
  (sqlup-blacklist '("source" "type" "date" "state"))
  :hook
  (sql-mode . sqlup-mode)
  (sql-interactive-mode . sqlup-mode))

(use-package sql-indent
  :after sql
  :hook
  (sql-mode . sqlind-minor-mode))

(provide 'lang-sql)
