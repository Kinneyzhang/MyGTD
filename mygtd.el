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

(require 'mygtd-face)
(require 'mygtd-macs)
(require 'mygtd-db)

;;;; Mygtd Task

(defvar mygtd-task-default-status "todo")

(defvar mygtd-task-org-todo "[ ]")

(defvar mygtd-task-org-done "[X]")

(defvar mygtd-task-icon-todo "□") ;; □ ■

(defvar mygtd-task-icon-done "■") ;; √ ☑ ▣

(defvar mygtd-task-icon-left1 "<")

(defvar mygtd-task-icon-right1 ">")

(defvar mygtd-task-icon-left2 "«")

(defvar mygtd-task-icon-right2 "»")

(defvar mygtd-daily-ewoc-data nil)

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

(defvar mygtd-daily-ewoc nil
  "Ewoc of mygtd daily buffer.")

(defvar mygtd-daily-date nil
  "Current date of mygtd daily buffer.")

(defvar mygtd-daily-buffer "*Mygtd Daily*")

(defvar mygtd-daily-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" #'mygtd-daily-show-previous)
    (define-key map "n" #'mygtd-daily-show-next)
    (define-key map "d" #'mygtd-daily-task-finish)
    (define-key map "u" #'mygtd-daily-task-undo)
    (define-key map "G" #'mygtd-daily-refresh)
    (define-key map "." #'mygtd-daily-goto-today)
    (define-key map "j" #'mygtd-daily-goto-date)
    (define-key map "a" #'mygtd-daily-task-add)
    (define-key map "D" #'mygtd-daily-task-delete)
    (define-key map "t" #'mygtd-daily-details-toggle)
    (define-key map "\C-x\C-q" #'mygtd-change-to-edit-mode)
    map))

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

(defun mygtd-task-status-icon (status)
  (pcase status ("todo" "□") ("done" "■")))

(defun mygtd-daily-pp (data)
  (if data
      (let* ((time (or (plist-get data :time)
                       mygtd-daily-date))
             (id (plist-get data :id))
             (icon (mygtd-task-status-icon (plist-get data :status)))
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
             ;; "• "
             (task-prefix (concat icon " "))
             (task-name name))
        (if mygtd-task-details-flag
            ;; show full details of tasks.
            (safe-insert (concat task-prefix task-name "  \n")
                         (propertize details 'face 'mygtd-task-details-face
                                     'line-prefix "  " 'wrap-prefix "  "))
          (let ((beg (point))
                end ov)
            (safe-insert (concat task-prefix task-name))
            (setq end (point))
            (setq ov (make-overlay beg end))
            (overlay-put ov 'id id))))
    (insert "No daily tasks.")))

;;; TODO: 每条task设置 id属性，在 edit mode 中编辑时，新的task要求不继承之前得id属性
;;; properties 似乎有问题，尝试 overlay 能否解决这个问题
;;; 考虑数据 增删改得情况下，id属性是否会有问题

;;; 每一行一条task，新的task赋予新的task id，入task表后，更新 record 表
;;; 切回daily mode 后重新生成ewoc数据

;;; prettify

(defvar mygtd-org-list-regexp
  "^ *\\([0-9]+[).]\\|[*+-]\\) \\(\\[[ X-]\\] \\)?"
  "Org list bullet and checkbox regexp.")

(defun mygtd-org-checkbox-fontify (checkbox)
  "Highlight org checkbox with NOTATION."
  (pcase checkbox
    ("[ ]"
     (add-text-properties (match-beginning 2) (1- (match-end 2))
                          `(display ,mygtd-task-icon-todo)))
    ("[X]"
     (add-text-properties (match-beginning 2) (1- (match-end 2))
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
    (setq major-mode 'mygtd-daily-mode)
    (setq mode-name "mygtd-daily")
    (use-local-map mygtd-daily-mode-map)))

;;;###autoload
(defun mygtd-daily-show (&optional date)
  "Show the view of mygtd-daily buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create mygtd-daily-buffer))
  (mygtd-daily-buffer-setup)
  (let* ((date (or date (format-time-string "%Y%m%d")))
         (ewoc (ewoc-create
                'mygtd-daily-pp
                (propertize (concat (mygtd-date-shown date) "\n")
                            'face '(bold :height 2.2)))))
    (setq mygtd-daily-date date)
    (setq mygtd-daily-ewoc ewoc)
    (if-let ((datas (mygtd-db-order-records date)))
        (dolist (data datas)
          (ewoc-enter-last ewoc data))
      (ewoc-enter-last ewoc nil))
    (setq mygtd-daily-ewoc-data (mygtd-ewoc-buffer-data))
    (mygtd-prettify-mode 1)
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
  (let* ((date mygtd-daily-date)
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
        (ewoc-invalidate mygtd-daily-ewoc node)))
    (mygtd-daily-ewoc-data-update)))

(defun mygtd-daily-ewoc-data-update ()
  (setq mygtd-daily-ewoc-data (mygtd-ewoc-buffer-data)))

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

;;; Add task by changing to mygtd-edit-mode

(defun mygtd-ewco-goto-first-node ()
  (ewoc-goto-node mygtd-daily-ewoc (ewoc-nth mygtd-daily-ewoc 0)))

(defun mygtd-change-to-edit-mode ()
  (interactive)
  (let ((old-data mygtd-daily-ewoc-data))
    (mygtd-edit-mode)
    ))

(defun mygtd-edit-finish ()
  (interactive)
  (message "Finish edit mode"))

(defun mygtd-edit-return ()
  (interactive)
  (newline)
  (insert mygtd-task-icon-todo " "))

(defun mygtd-edit-move-up ()
  (interactive)
  (let ((curr-ov (overlays-at (point)))
        )))

(defun mygtd-edit-move-down ()
  (interactive))

(defvar mygtd-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'mygtd-edit-finish)
    (define-key map (kbd "M-<RET>") #'mygtd-edit-return)
    (define-key map (kbd "M-n") #'mygtd-edit-move-up)
    (define-key map (kbd "M-p") #'mygtd-edit-move-down)
    map))

(define-derived-mode mygtd-edit-mode fundamental-mode "Mygtd Edit"
  (interactive)
  (setq major-mode 'mygtd-edit-mode)
  (setq mode-name "mygtd-edit")
  (use-local-map mygtd-edit-mode-map)
  (mygtd-edit-highlight-keywords)
  (read-only-mode -1))

(defun mygtd-edit-highlight-keywords ()
  "Highlight keywords in mygtd-edit-mode buffer."
  (font-lock-add-keywords
   nil
   '(("^[0-9]\\{4\\}" . 'font-lock-constant-face)))
  (font-lock-mode 1))

;; (defun mygtd-buffer-p ()
;;   (or (string= (buffer-name) mygtd-daily-buffer)
;;       (string= (buffer-name) mygtd-capture-buffer)))

(define-minor-mode mygtd-prettify-mode
  "Minor mode for mygtd-daily."
  :keymap nil
  (when (mygtd-buffer-p)
    (if mygtd-prettify-mode
        (progn
          (add-hook 'window-configuration-change-hook #'mygtd-preserve-window-margin)
          (hl-line-mode 1))
      (remove-hook 'window-configuration-change-hook #'mygtd-preserve-window-margin))
    (jit-lock-refontify)))

(provide 'mygtd)

