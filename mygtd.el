(require 'org-id)
(require 'mygtd-macs)
(require 'mygtd-db)

(defvar mygtd-task-default-status "todo")

(defvar mygtd-daily-ewoc nil)

(defvar mygtd-daily-buffer "*Mygtd Daily*")

(defun mygtd-task-icon (status)
  (pcase status
    ("todo" "▢")
    ("done" "√")))

;; if has not timestr, according to todo or done in status
;; if has timestr, compare current and the next timestr
;;   if wide(curr) > wide(next): migrate: > 
;;   if wide(curr) < wide(next): someday: <
;;   if year/month/date(curr) < year/month/date(next): migrate: >

(defvar mygtd-daily-date nil
  "Current date of mygtd daily buffer.")

;; 2022
;; 202209
;; 20220910

(defvar mygtd-icon-todo "▢")
(defvar mygtd-icon-done "√")
(defvar mygtd-icon-migrate ">")
(defvar mygtd-icon-someday "<")

(defun mygtd-migrated-icon (from-time to-time)
  "Return the icon of migrate or someday status 
according to FROM-TIME and TO-TIME."
  (let ((from-len (length from-time))
        (to-len (length to-time)))
    (pcase from-len
      ;; e.g. 20220914 -> 202209 = someday(<)
      ((pred (< to-len)) mygtd-icon-someday)
      ;; e.g. 202209 -> 20220914 = migrate(>)
      ((pred (> to-len)) mygtd-icon-migrate)
      ;; e.g. 20220913 -> 20220914 = migrate(>)
      (_ mygtd-icon-migrate))))

;; (mygtd-migrated-icon "20220914" "202209")
;; (mygtd-migrated-icon "202209" "20220914")
;; (mygtd-migrated-icon "20220913" "20220914")


(defun mygtd-daily-pp (data)
  ;; timestr should contains mygtd-daily-date
  (let* ((curr-time mygtd-daily-date)
         (id (plist-get data :id))
         (status (plist-get data :status))
         (name (plist-get data :name))
         (category (plist-get data :category))
         (timestr (plist-get data :timestr))
         (timelst (when timestr (split-string timestr  "," t " +")))
         (curr-nth (seq-position timelst curr-time))
         (length (length timelst)))
    (if (= curr-nth (1- length))
        ;; current date is the last one
        (insert (format "%s %s" (mygtd-task-icon status) name))
      ;; current date is not the last one.
      ;; compare curr-time and next-time
      (let* ((next-time (nth (1+ curr-nth) timelst))
             (icon (mygtd-migrated-icon curr-time next-time)))
        (insert (format "%s %s" icon name))))))

(defun mygtd-add-task (plist)
  (let-alist (plist->alist plist)
    (let ((.:id (or .:id (org-id-uuid)))
          (.:status (or .:status mygtd-task-default-status)))
      (mygtd-db-query
       `[:insert :into task
                 :values ([,.:id ,.:name ,.:category ,.:status
                                 ,.:timestr ,.:period ,.:deadline
                                 ,.:location ,.:device ,.:parent])]))))

(defun mygtd-add-multi-tasks (list)
  (dolist (plist list)
    (mygtd-add-task plist)))

(defvar mygtd-daily-mode-map nil)
(defun mygtd-buffer-setup ()
  (let ((inhibit-read-only t))
    (kill-all-local-variables)
    (setq major-mode 'mygtd-daily-mode
          mode-name "Mygtd Daily")
    (use-local-map mygtd-daily-mode-map)
    (erase-buffer)
    (buffer-disable-undo)))

(defun mygtd-daily-task (date)
  (mygtd-db-query
   `[:select * :from task :where (like timestr ,(concat "%" date "%"))]))
(mygtd-daily-task "20220915")

(defun mygtd-monthly-task (month)
  (mygtd-db-query
   `[:select * :from task :where (or (like timestr ,(concat "%" month ",%"))
                                     (like timestr ,(concat "%" month)))]))

(mygtd-monthly-task "202209")

(defun mygtd-yearly-task (year)
  (mygtd-db-query
   `[:select * :from task :where (or (like timestr ,(concat "%" year ",%"))
                                     (like timestr ,(concat "%" year ",%")))]))

(mygtd-add-multi-tasks
 '((:name "test1" :category "work" :timestr "20220914,20220915")
   (:name "test2" :category "study" :timestr "20220915,20220916")
   (:name "test3" :category "work" :timestr "20220914")
   (:name "test4" :category "work" :timestr "202209,20220915")
   (:name "test5" :category "work" :timestr "20220916")
   (:name "test6" :category "study" :timestr "20220915,202209")
   (:name "test7" :category "work" :timestr "20220915")
   (:name "test8" :category "work" :timestr "20220915,202209,202210")
   (:name "test9" :category "work" :timestr "2022")
   (:name "test10" :category "work" :timestr "2022,2023")
   (:name "test11" :category "work" :timestr "202209,2022")
   (:name "test12" :category "work" :timestr "202209,2022,202210")))

(defun mygtd-daily-view-show ()
  "Show the view of mygtd-daily buffer."
  (interactive)
  (if-let ((buf (get-buffer mygtd-daily-buffer)))
      (switch-to-buffer buf)
    (switch-to-buffer (get-buffer-create mygtd-daily-buffer))
    (mygtd-buffer-setup)

    (let ((ewoc (ewoc-create 'mygtd-daily-pp
                             (propertize "Mygtd Daily\n" 'face '(:height 1.5)))))
      (set (make-local-variable 'mygtd-daily-ewoc) ewoc)
      
      (ewoc-enter-last ewoc data))
    (read-only-mode 1)
    (mygtd-mode 1)))

(define-minor-mode mygtd-mode
  "Minor mode for mygtd-daily."
  :lighter " Mygtd"
  :keymap (let ((map (make-sparse-keymap))) map)
  :require 'mygtd
  (if mygtd-mode
      (progn
        (add-hook 'window-configuration-change-hook #'mygtd-preserve-window-margin)
        (hl-line-mode 1))
    (remove-hook 'window-configuration-change-hook #'mygtd-preserve-window-margin)))

(provide 'mygtd)
