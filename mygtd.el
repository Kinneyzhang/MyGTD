;;; mygtd.el --- mygtd functions.  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'org-id)

(require 'mygtd-macs)
(require 'mygtd-db)

;;;; Mygtd Task

(defvar mygtd-task-default-status "todo")

(defvar mygtd-task-org-todo "[ ]")

(defvar mygtd-task-org-done "[X]")

(defvar mygtd-task-icon-todo "□") ;; ▢ ☐ □

(defvar mygtd-task-icon-done "■") ;; √ ☑ ▣

(defvar mygtd-task-icon-left1 "<")

(defvar mygtd-task-icon-right1 ">")

(defvar mygtd-task-icon-left2 "«")

(defvar mygtd-task-icon-right2 "»")

(defvar-local mygtd-daily-ewoc-data nil)

;; ▶▷◀◁
;; ■▢▣□
;; ☐☒☑

;; 获取一个ewoc buffer里面的所有 data list，拼成 idstr
(defun mygtd-task-order-update (time)
  "Add or update the task order table according to TIME
 and current tasks in buffer."
  (let ((idstr )))
  (mygtd-db-query
   `[:insert :into order :values ([,time])]))

(defun mygtd-task-add (plist)
  "Add a task to database according to a PLIST.
1. Add the basic task info into task table.
2. Add a record into migrate table.
3. Add or update the order table according to the time in plist."
  (let-alist (plist->alist plist)
    (let* ((.:id (or .:id (org-id-uuid)))
           (.:status (or .:status mygtd-task-default-status))
           (data (list .:id .:name .:category .:status .:details .:period
                       .:deadline .:location .:device .:parent)))
      (mygtd-db-query `[:insert :into task :values ([,@data])])
      (mygtd-db-query `[:insert :into migrate :values ([,.:id ,.:time])])
      data)))

(defun mygtd-task-multi-add (plist-list)
  "Add multiple tasks to database according to a PLIST-LIST."
  (dolist (plist plist-list)
    (mygtd-task-add plist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mygtd Daily

(defvar mygtd-daily-ewoc nil)

(defvar mygtd-daily-date nil
  "Current date of mygtd daily buffer.")

(defvar mygtd-daily-buffer "*Mygtd Daily*")

;; (defvar mygtd-daily-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "p" #'mygtd-daily-show-previous)
;;     (define-key map "n" #'mygtd-daily-show-next)
;;     (define-key map "d" #'mygtd-daily-task-finish)
;;     (define-key map "u" #'mygtd-daily-task-undo)
;;     (define-key map "G" #'mygtd-daily-refresh)
;;     (define-key map "." #'mygtd-daily-goto-today)
;;     (define-key map "j" #'mygtd-daily-goto-date)
;;     (define-key map "a" #'mygtd-daily-task-add)
;;     (define-key map "D" #'mygtd-daily-task-delete)
;;     (define-key map "t" #'mygtd-daily-details-toggle)
;;     map))

;; (defvar mygtd-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "p" #'mygtd-daily-show-previous)
;;     (define-key map "n" #'mygtd-daily-show-next)
;;     (define-key map "d" #'mygtd-daily-task-finish)
;;     (define-key map "u" #'mygtd-daily-task-undo)
;;     (define-key map "G" #'mygtd-daily-refresh)
;;     (define-key map "." #'mygtd-daily-goto-today)
;;     (define-key map "j" #'mygtd-daily-goto-date)
;;     (define-key map "a" #'mygtd-daily-task-add)
;;     (define-key map "D" #'mygtd-daily-task-delete)
;;     (define-key map "t" #'mygtd-daily-details-toggle)
;;     map))

;; (:id \"111\" :name \"test111\" :category \"work\" :status \"todo\" :period nil :deadline nil :location nil :device nil :parent nil)

(defvar mygtd-task-details-flag nil
  "Determine whether to show details of task.")

(defun mygtd-task-icon (id time)
  "Return task icon with task ID and TIME."
  (let* ((timelst (mygtd-db-migrate-timelst id))
         (len (length timelst))
         (nth (seq-position timelst time)))
    (if (= nth (1- len))
        mygtd-task-icon-todo
      (let* ((from-time time)
             (to-time (seq-elt timelst (1+ nth))))
        (if (= (length from-time) (length to-time))
            (if (mygtd-time-less-p from-time to-time)
                mygtd-task-icon-right1
              mygtd-task-icon-left1)
          (if (mygtd-time-wider-p from-time to-time)
              mygtd-task-icon-right2
            mygtd-task-icon-left2))))))

(defun mygtd-task-status-text (status)
  (pcase status
    ("todo" "[ ]")
    ("done" "[X]")))

(defun mygtd-daily-pp (data)
  (if data
      (let* ((time (or (plist-get data :time)
                       mygtd-daily-date))
             (id (plist-get data :id))
             (icon (mygtd-task-icon id time))
             (name (propertize (plist-get data :name)
                               'face 'mygtd-task-name-face))
             (category (plist-get data :category))
             (status (plist-get data :status))
             (status-text (mygtd-task-status-text status))
             (details (propertize (plist-get data :details)
                                  'face 'mygtd-task-details-face))
             (period (plist-get data :period))
             (deadline (plist-get data :deadline))
             (location (plist-get data :location))
             (device (plist-get data :device))
             (parent (plist-get data :parent))
             (task-basic-text (propertize (format "- %s %s" status-text name)
                                          'icon icon 'task-id id)))
        (if mygtd-task-details-flag
            ;; show full details of tasks.
            (safe-insert task-basic-text "  " category "/" location "/" device "\n"
                         (propertize details 'line-prefix "  " 'wrap-prefix "  "))
          (safe-insert task-basic-text)))
    (insert "No daily tasks.")))

;;; prettify

(defvar mygtd-org-list-regexp
  "^ *\\([0-9]+[).]\\|[*+-]\\) \\(\\[[ X-]\\] \\)?"
  "Org list bullet and checkbox regexp.")

(defun mygtd-org-checkbox-fontify (checkbox)
  "Highlight org checkbox with NOTATION."
  (pcase checkbox
    ("[ ]"
     (let ((icon (get-text-property (point) 'icon)))
       (add-text-properties (match-beginning 2) (1- (match-end 2))
                            `(display ,icon))))
    ("[X]"
     (add-text-properties
      (match-beginning 2) (1- (match-end 2))
      `(display ,mygtd-task-icon-done)))))

(defun mygtd-org-list-fontify (beg end)
  "Highlight org list bullet between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward mygtd-org-list-regexp end t)
      (with-silent-modifications
        (add-text-properties (match-beginning 1) (match-end 1) '(display "•"))
        (when (match-beginning 2)
          (pcase (match-string-no-properties 2)
            ;; ("[-] " (mygtd-org-checkbox-fontify "☐"))
            ("[ ] " (mygtd-org-checkbox-fontify mygtd-task-org-todo))
            ("[X] " (mygtd-org-checkbox-fontify mygtd-task-org-done))))))))

(defun mygtd-org-list-unfontify (beg end)
  "Unhighlight org list bullet between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward mygtd-org-list-regexp end t)
      (with-silent-modifications
        (add-text-properties (match-beginning 1) (match-end 1) '(display nil))
        (when (match-beginning 2)
          (add-text-properties (match-beginning 2) (1- (match-end 2)) '(display nil)))))))

(defun mygtd-daily-prettify ()
  "Prettify the buffer of mygtd daily."
  )

(defun mygtd-daily-buffer-setup ()
  "Setup the buffer of `mygtd-daily-mode'."
  (let ((inhibit-read-only t))
    (kill-all-local-variables)
    (erase-buffer)
    (buffer-disable-undo)
    ;; (mygtd-daily-mode)
    ))

;;;###autoload
(defun mygtd-daily-show (&optional date)
  "Show the view of mygtd-daily buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create mygtd-daily-buffer))
  (mygtd-daily-buffer-setup)
  (let* ((date (or date (format-time-string "%Y%m%d")))
         (ewoc (ewoc-create
                'mygtd-daily-pp
                (concat (propertize (concat "Mygtd Daily\n\n") 'face '(:height 1.5))
                        (propertize (concat (mygtd-date-shown date) "\n")
                                    'face '(:height 1.1))))))
    (setq mygtd-daily-date date)
    (set (make-local-variable 'mygtd-daily-ewoc) ewoc)
    (if-let ((datas (mygtd-db-order-records date)))
        (dolist (data datas)
          (ewoc-enter-last ewoc data))
      (ewoc-enter-last ewoc nil))
    (setq mygtd-daily-ewoc-data (mygtd-ewoc-buffer-data))
    (mygtd-mode 1)
    (read-only-mode 1)))

;;;###autoload
(defun mygtd-daily-show-next ()
  "Switch to the view of next date"
  (interactive)
  (mygtd-daily-show
   (format-time-string "%Y%m%d" (+ (mygtd-date-to-second mygtd-daily-date)
                                   (* 24 60 60)))))

;;;###autoload
(defun mygtd-daily-show-previous ()
  "Switch to the view of previous date"
  (interactive)
  (mygtd-daily-show
   (format-time-string "%Y%m%d" (- (mygtd-date-to-second mygtd-daily-date)
                                   (* 24 60 60)))))

(defun mygtd-daily-details-toggle ()
  "Toggle for showing or hiding mygtd daily task details."
  (interactive)
  (if mygtd-task-details-flag
      (setq mygtd-task-details-flag nil)
    (setq mygtd-task-details-flag t))
  (ewoc-refresh mygtd-daily-ewoc))

;;;###autoload
(defun mygtd-daily-refresh ()
  "Force to refresh mygtd daily ewoc buffer."
  (interactive)
  (mygtd-daily-show mygtd-daily-date))

;;;###autoload
(defun mygtd-daily-task-finish (&optional task-id)
  "Update the status to done for task with TASK-ID or task at point."
  (interactive)
  (when-let ((id (or task-id (mygtd-ewoc-pos-prop :id))))
    (mygtd-ewoc-pos-update :status "done")
    (mygtd-db-query `[:update task :set (= status "done") :where (= id ,id)])))

;;;###autoload
(defun mygtd-daily-task-undo (&optional task-id)
  "Update the status to todo for task with TASK-ID or task at point."
  (interactive)
  (when-let ((id (or task-id (mygtd-ewoc-pos-prop :id))))
    (mygtd-ewoc-pos-update :status "todo")
    (mygtd-db-query `[:update task :set (= status "todo") :where (= id ,id)])))

;;;###autoload
(defun mygtd-daily-goto-today ()
  "Switch to today's mygtd daily buffer."
  (interactive)
  (let ((today (format-time-string "%Y%m%d")))
    (mygtd-daily-show today)))

;;;###autoload
(defun mygtd-daily-goto-date ()
  "Jump to a specific date by choosing in a calendar."
  (interactive)
  (let* ((curr-date (mygtd-date-to-second mygtd-daily-date))
         (org-date (org-read-date nil nil nil nil curr-date))
         (date (string-join (split-string org-date "-" t))))
    (mygtd-daily-show date)))

;;;###autoload
(defun mygtd-daily-task-add ()
  (interactive)
  (let ((date mygtd-daily-date)
        (name (completing-read "Input the task name: " nil))
        (category (completing-read "Input the task category: " nil))
        (details (completing-read "Input details of task: " nil))
        (data (mygtd-task-add (list :name name :category category
                                    :details details :time date)))
        (plist (mygtd-query-wrapper 'task data)))
    (if (mygtd-ewoc-buffer-data)
        (ewoc-enter-last mygtd-daily-ewoc plist)
      ;; no task
      (let ((node (ewoc-nth mygtd-daily-ewoc 0)))
        (ewoc-set-data node plist)
        (ewoc-invalidate mygtd-daily-ewoc node)))))

;;;###autoload
(defun mygtd-daily-task-delete ()
  "Delete a daily task."
  (interactive)
  (if-let ((id (plist-get (mygtd-ewoc-pos-data) :id))
           (inhibit-read-only t))
      (progn
        (ewoc-delete mygtd-daily-ewoc (mygtd-ewoc-pos-node))
        (mygtd-db-task-delete id)
        (unless (mygtd-ewoc-buffer-data)
          (ewoc-enter-last mygtd-daily-ewoc nil)))
    (message "No task to delete!")))

(defun mygtd-daily-edit-details ()
  (interactive)
  )

;;; switch to mygtd-edit-mode to add, delete or update task.
;; when use mygtd-edit-mode: switch to a editable org-mode buffer.

(defun mygtd-edit-mode ()
  )

(defvar mygtd-daily-old-data nil
  "The old ewoc buffer data before editing.")

(defvar mygtd-daily-new-data nil
  "The new ewoc buffer data after editing.")

(defun mygtd-change-to-edit-mode ()
  (interactive)
  (unless (derived-mode-p 'mygtd-daily-mode)
    (error "Not a mygtd daily buffer."))
  (setq mygtd-daily-old-data mygtd-daily-ewoc-data)
  ;; (mygtd-edit-mode)
  )

;; (defun mygtd-toggle-read-only ()
;;   (interactive)
;;   (if (derived-mode-p 'mygtd-daily-mode)
;;       (mygtd-change-to-edit-mode)
;;     (read-only-mode 'toggle)))

;; (define-derived-mode mygtd-daily-mode org-mode "mygtd-daily"
;;   (interactive)
;;   (setq major-mode 'mygtd-daily-mode)
;;   (setq mode-name "mygtd-daily")
;;   (use-local-map mygtd-daily-mode-map)
;;   (run-hooks 'mygtd-daily-mode-hook))

;; (define-derived-mode mygtd-edit-mode org-mode "mygtd-edit"
;;   (interactive)
;;   (setq major-mode 'mygtd-edit-mode)
;;   (setq mode-name "mygtd-edit")
;;   (read-only-mode -1)
;;   (use-local-map mygtd-edit-mode-map)
;;   (run-hooks 'mygtd-edit-mode-hook))

(defun mygtd-buffer-p ()
  (or (string= (buffer-name) mygtd-daily-buffer)))

(define-minor-mode mygtd-mode
  "Minor mode for mygtd-daily."
  :lighter " Mygtd"
  :keymap (let ((map (make-sparse-keymap))) map)
  :require 'mygtd
  :global t
  (if mygtd-mode
      (when (mygtd-buffer-p)
        (jit-lock-register #'mygtd-org-list-fontify)
        (mygtd-org-list-fontify (point-min) (point-max))
        (add-hook 'window-configuration-change-hook #'mygtd-preserve-window-margin)
        (hl-line-mode 1))
    (when (mygtd-buffer-p)
      (jit-lock-unregister #'mygtd-org-list-fontify)
      (mygtd-org-list-unfontify (point-min) (point-max))
      (remove-hook 'window-configuration-change-hook #'mygtd-preserve-window-margin))))

(provide 'mygtd)

