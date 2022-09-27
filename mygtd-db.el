;;; mygtd-db.el --- Database API for mygtd functions.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/md-wiki
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This Module implements some database API for mygtd functions.

;;; Code:

(require 'promise)
(require 'emacsql)
(require 'emacsql-sqlite)

(defvar mygtd-db-file
  (expand-file-name "mygtd.db" (concat user-emacs-directory "mygtd"))
  "File path of mygtd database.")

(defvar mygtd-db--conn (make-hash-table :test #'equal)
  "Database connection to mygtd-db.")

(defconst mygtd-db--table-schemata
  '((task
     [(id :primary-key) (name :not-null) (category :not-null)
      (status :not-null) (period) (deadline)
      (location) (device) (parent)])
    (migrate
     [(id :not-null) (time :not-null)] ;; year month day
     (:foreign-key [id] :references task [id] :on-delete :cascade))
    (order
     [(time :not-null) (idstr :not-null)])
    (project
     [(id :primary-key) (name :not-null) (status :not-null)
      (deadline) (parent)])))

(defun mygtd-db--get-conn ()
  "Return the mygtd database connection with key PATH."
  (gethash mygtd-db-file mygtd-db--conn))

(defun mygtd-db--init (db)
  "Initialize database DB with `mygtd-db--table-schemata'."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) mygtd-db--table-schemata)
      (emacsql db `[:create-table ,table ,schema]))))

(defun mygtd-db ()
  "Entrypoint to mygtd sqlite database."
  (unless (and (mygtd-db--get-conn)
               (emacsql-live-p (mygtd-db--get-conn)))
    (let ((init-db (not (file-exists-p mygtd-db-file))))
      (make-directory (file-name-directory mygtd-db-file) t)
      (let ((conn (emacsql-sqlite mygtd-db-file)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash mygtd-db-file conn mygtd-db--conn)
        (when init-db
          (mygtd-db--init conn)))))
  (mygtd-db--get-conn))

(defun mygtd-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for current mygtd db."
  (unless db
    (setq db (mygtd-db--get-conn)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun mygtd-db-query (sql &rest args)
  "Return SQL query on mygtd database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if (stringp sql)
      (emacsql (mygtd-db) (apply #'format sql args))
    (apply #'emacsql (mygtd-db) sql args)))

(defun mygtd-db-clear ()
  "Clear all data in mygtd database."
  (interactive)
  (when (file-exists-p mygtd-db-file)
    (dolist (table (mapcar #'car mygtd-db--table-schemata))
      (mygtd-db-query `[:delete :from ,table]))))

(defun mygtd-db-drop ()
  "Drop the whole mygtd database."
  (interactive)
  (mygtd-db--close)
  (delete-file mygtd-db-file))

;;; Specific table query

(defun mygtd-db-order-idstr (time)
  (promise-new
   (lambda (resolve _reject)
     (funcall resolve (caar (mygtd-db-query `[:select idstr :from order
                                                      :where (= time ,time)]))))))
;; (mygtd-order-table-records "20220926")

(defun mygtd-db-task-records (id)
  "Return the records of task with task ID."
  (car (mygtd-query-result-plist
        'task (mygtd-db-query `[:select * :from task :where (= id ,id)]))))

;; (mygtd-db-task-records "111")
;; (mygtd-db-task-records "222")

(provide 'mygtd-db)
