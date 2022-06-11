(require 'emacsql)
(require 'emacsql-sqlite)

(defvar mygtd-db-file
  (expand-file-name "mygtd.db" (concat user-emacs-directory "mygtd"))
  "File path of mygtd database.")

(defvar mygtd-db--conn (make-hash-table :test #'equal)
  "Database connection to mygtd-db.")

(defconst mygtd-db--table-schemata
  '((note
     [(id :primary-key)
      (content :not-null)
      (timestamp :not-null)
      (parent-id) ;; parent may be a todo entry
      (stick) ;; whether to stick on top
      (reminder)]) ;; a special note, 有待办的属性，但又不是强任务
    (entry
     [(id :primary-key)
      (name :not-null)
      (type :not-null) ;; task, proj, area, 
      (status :not-null) ;; todo, done, nil
      (date)
      (period) ;; morning, afternoon, evening
      (effort)
      (order :not-null)
      (level :not-null)
      (clocking)
      (parent-id)])))

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

(defun mygtd-add-entry ()
  (interactive)
  )

(defun mygtd-add-note ()
  (interactive)
  )

(defun mygtd-add-task ()
  (interactive))

(defun mygtd-add-proj ())


(provide 'mygtd-db)
