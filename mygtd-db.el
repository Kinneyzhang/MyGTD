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

(require 'emacsql)
(require 'emacsql-sqlite)

(defvar mygtd-db-file
  (expand-file-name "mygtd.db" (concat user-emacs-directory "mygtd"))
  "File path of mygtd database.")

(defvar mygtd-db--conn (make-hash-table :test #'equal)
  "Database connection to mygtd-db.")

(defconst mygtd-db--table-schemata
  '((project
     [(id :primary-key) (name :not-null) (status :not-null)
      (deadline) (parent)])
    (task
     [(id :primary-key) (name :not-null) (category :not-null)
      (status :not-null) (details) (period) (deadline)
      (location) (device) (parent)]
     (:foreign-key [parent] :references project [id] :on-delete :cascade))
    (migrate ;; 每次迁移会产生一条迁移记录
     [(id :not-null) (time :not-null)] ;; year month day
     (:foreign-key [id] :references task [id] :on-delete :cascade))
    (order
     [(time :not-null) (idstr :not-null)])))

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

(defun mygtd-db-task-records (id)
  "Return the records of task with task ID.
Result example: (:id \"111\" :name \"test111\" :category \"work\" :status \"todo\" :period nil :deadline nil :location nil :device nil :parent nil)"
  (car (mygtd-query-wrapper
        'task (mygtd-db-query `[:select * :from task :where (= id ,id)]))))

(defun mygtd-db-task-delete (id)
  (mygtd-db-query `[:delete :from task :where (= id ,id)]))

;; migrate table

(defun mygtd-db-migrate-tasks (time)
  "Return a list of task id at a specific TIME."
  (mapcar #'car (mygtd-db-query `[:select id :from migrate
                                          :where (= time ,time)])))

(defun mygtd-db-migrate-timelst (id)
  "Return all task migration record with task id ID."
  (mapcar #'car (mygtd-db-query
                 `[:select time :from migrate :where (= id ,id)])))

(defun mygtd-task-icon (id time)
  "Return task icon with task ID and TIME."
  (let* ((timelst (mygtd-db-migrate-timelst id))
         (len (length timelst))
         (nth (seq-position timelst time)))
    (if (= nth (1- len))
        (progn
          mygtd-task-icon-todo)
      (let* ((from-time time)
             (to-time (seq-elt timelst (1+ nth))))
        (if (= (length from-time) (length to-time))
            (if (mygtd-time-less-p from-time to-time)
                mygtd-task-icon-right1
              mygtd-task-icon-left1)
          (if (mygtd-time-wider-p from-time to-time)
              mygtd-task-icon-right2
            mygtd-task-icon-left2))))))

(defun mygtd-db-migrate-records (id)
  "Return the migration records according to task ID."
  (mygtd-query-wrapper
   'migrate
   (mygtd-db-query `[:select * :from migrate :where (= id ,id)])))

;; when you create a new task, it should add a record both in 'task' and 'migrate' table

(defun mygtd-db-order-idstr (time)
  "Return task id string at a specific TIME."
  (caar (mygtd-db-query `[:select idstr :from order
                                  :where (= time ,time)])))

(defun mygtd-db-order-idlst (time)
  "Return task id list at a specific TIME."
  (when-let ((idstr (mygtd-db-order-idstr time)))
    (mygtd-idstr->idlst idstr)))

(defun mygtd-db-order-add (time idstr)
  (mygtd-db-query `[:insert-into order :values ([,time ,idstr])]))

(defun mygtd-db-order-update (time idstr)
  (mygtd-db-query `[:update order :set (= idstr ,idstr) :where (= time ,time)]))

(defun mygtd-db-order-records (time)
  "Return a list of records on specific TIME."
  (let* ((curr-idlst (mygtd-db-order-idlst time))
         (idlst (mygtd-db-migrate-tasks time)))
    (if curr-idlst
        (when (or (seq-difference curr-idlst idlst)
                  (seq-difference idlst curr-idlst))
          ;;; if there are differences between current order idlst
          ;;; and idlist in migration table, use idlst in migration table.
          ;;; Otherwise, use idlst in order table.
          ;; (message "diff: update order table")
          (setq curr-idlst idlst)
          (mygtd-db-order-update time (mygtd-idlst->idstr curr-idlst)))
      ;;; no record in 'order' table, add new according to migration table.
      ;; (message "none: insert order table")
      (setq curr-idlst idlst)
      (mygtd-db-order-add time (mygtd-idlst->idstr curr-idlst)))
    ;;; return the id list data according to order.
    (mapcar #'mygtd-db-task-records curr-idlst)))

(provide 'mygtd-db)
