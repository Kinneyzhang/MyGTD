;; (require 'mygtd-macs)
;; (require 'mygtd-db)

;; (defvar mygtd-daily-buf "*Mygtd Daily*"
;;   "Buffer name of mygtd-daily buffer.")

;; (defvar mygtd-daily-data nil
;;   "The ewoc data list of mygtd-daily.
;; Each data is a plist.")


;; (defun mygtd-entry-init (id name type status date period
;;                             effort order level clocking parent-id)
;;   `[,id ,name ,type ,status ,date ,period ,effort ,order ,level ,clocking ,parent-id])

;; (defun mygtd-entry-add (vector)
;;   "Add a task to do today."
;;   (mygtd-db-query
;;    `[:insert :into entry :values (,vector)]))

;; (defvar mygtd-task-periods '("morning" "afternoon" "evening"))

;; (defun mygtd-get-projects ()
;;   "Return all active projects."
;;   (mygtd-db-query [:select [id name level] :from entry
;;                            :where (and (= type "proj")
;;                                        ())]))

;; (defun mygtd-get-level ()
;;   )

;; (defun mygtd-daily-add-task ()
;;   "Remove a task to do today."
;;   (let* ((id (org-id-uuid))
;;          (name (completing-read "Input a task name: " nil))
;;          (type "task")
;;          (status "todo")
;;          (date (format-time-string "%Y-%m-%d"))
;;          (period (completing-read "Choose a period: " mygtd-task-periods nil t))
;;          (effort (completing-read "Input the effort(e.g. 2h 30m): " nil))
;;          (parent-id (completing-read "Choose a parent(RET to skip): " ))
;;          (order ())
;;          (clocking nil)
;;          (level)
;;          (vector `[,id ,name ,type ,status ,date ,period ,effort
;;                        ,order ,level ,clocking ,parent-id]))
;;     (mygtd-entry-add vector)))

;; (defun mygtd-daily-edit-task ()
;;   "Edit a task to do today."
;;   )


;; (defun mygtd-daily-ewoc-pp (i)
;;   "Pretty printer function for mygtd-data-ewoc."
;;   (let* ((plst (nth i mygtd-daily-data))
;;          (name (plist-get plst :name))
;;          (status (plist-get plst :status))
;;          (icon (mygtd-status-icon status))
;;          (clocking (plist-get plst :clocking)))
;;     (insert icon " " name
;;             (if clocking (propertize " ⏰ " 'face 'bold) "")
;;             "\n")))

;; (defun mygtd-daily-get-tasks (&optional date)
;;   "Return all daily tasks on date.
;; If DATE is nil, the default one is today."
;;   (mygtd-db-query `[:select [status name clocking] :from entry
;;                             :where (and (= date ,date)
;;                                         (= type "task"))
;;                             :order-by (asc order)]))

;; (defun mygtd-daily-view-show (&optional date)
;;   "Show the view of mygtd-daily."
;;   (interactive)
;;   (if-let ((buf (get-buffer mygtd-daily-buf)))
;;       (switch-to-buffer buf)
;;     (let ((buf (get-buffer-create mygtd-daily-buf)))
;;       (setq mygtd-daily-data (mygtd-get-daily-tasks date))
;;       (with-current-buffer buf
;;         (mygtd-buf-setup)
;;         (dotimes (i (length mygtd-daily-data))
;;           (ewoc-enter-last mygtd-ewoc i))
;;         (read-only-mode 1)
;;         (mygtd-daily-mode 1))
;;       (switch-to-buffer buf))))

;; (define-minor-mode mygtd-daily-mode
;;   ""
;;   :lighter " Daily"
;;   :keymap (let ((map (make-sparse-keymap)))
;;             map)
;;   :require 'mygtd
;;   (if mygtd-daily-mode
;;       (progn
;;         (add-hook 'window-configuration-change-hook #'mygtd-preserve-window-margin)
;;         (hl-line-mode 1))
;;     (remove-hook 'window-configuration-change-hook #'mygtd-preserve-window-margin)))

;; 周规划

;; 月规划
