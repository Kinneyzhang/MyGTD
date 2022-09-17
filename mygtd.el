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

(defvar mygtd-task-icon-todo "☐") ;; ▢

(defvar mygtd-task-icon-done "☑") ;; √

(defvar mygtd-task-icon-migrate ">")

(defvar mygtd-task-icon-someday "<")

(defun mygtd-task-interval-query (time)
  "Return a interval of all tasks.
The interval could be daily, monthly or yearly."
  (mygtd-query-result-plist
   'task (mygtd-db-query
          `[:select * :from task :where (like timestr ,(concat "%" time ",%"))])))

(defun mygtd-task-add (plist)
  "Add a task to database according to a PLIST."
  (let-alist (plist->alist plist)
    (let ((.:id (or .:id (org-id-uuid)))
          (.:status (or .:status mygtd-task-default-status)))
      (mygtd-db-query
       `[:insert :into task
                 :values ([,.:id ,.:name ,.:category ,.:status
                                 ,.:timestr ,.:period ,.:deadline
                                 ,.:location ,.:device ,.:parent])]))))

(defun mygtd-task-multi-add (plist-list)
  "Add multiple tasks to database according to a PLIST-LIST."
  (dolist (plist plist-list)
    (mygtd-task-add plist)))

;;; task icon

(defun mygtd-task-icon (status)
  "Icon for each status."
  (pcase status
    ("todo" mygtd-task-icon-todo)
    ("done" mygtd-task-icon-done)))

(defun mygtd-migrated-icon (from-time to-time)
  "Return the icon of migrate or someday status 
according to FROM-TIME and TO-TIME."
  ;; if has not timestr, according to todo or done in status
  ;; if has timestr, compare current and the next timestr
  ;;   if wide(curr) > wide(next): migrate: > 
  ;;   if wide(curr) < wide(next): someday: <
  ;;   if year/month/date(curr) < year/month/date(next): migrate: >
  (let ((from-len (length from-time))
        (to-len (length to-time)))
    (pcase from-len
      ;; e.g. 20220914 -> 202209 = someday(<)
      ((pred (< to-len)) mygtd-task-icon-someday)
      ;; e.g. 202209 -> 20220914 = migrate(>)
      ((pred (> to-len)) mygtd-task-icon-migrate)
      ;; e.g. 20220913 -> 20220914 = migrate(>)
      (_ mygtd-task-icon-migrate))))


;;;; Mygtd Daily

(defvar mygtd-daily-ewoc nil)

(defvar mygtd-daily-date nil
  "Current date of mygtd daily buffer.")

(defvar mygtd-daily-buffer "*Mygtd Daily*")

;; (defface mygtd-icon-done-face
;;   '((t warning)))

;; (defvar mygtd-icon-todo-face
;;   '((t warning)))

(defun mygtd-daily-pp (data)
  ;; timestr should contains mygtd-daily-date
  (if data
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
            (insert (format "• %s %s" (mygtd-task-icon status) name))
          ;; current date is not the last one.
          ;; compare curr-time and next-time
          (let* ((next-time (nth (1+ curr-nth) timelst))
                 (icon (mygtd-migrated-icon curr-time next-time)))
            (insert (format "• %s %s" icon name)))))
    (insert "No daily tasks.")))

(defvar mygtd-daily-mode-map nil)

(defun mygtd-daily-buffer-setup ()
  "Setup the buffer of `mygtd-daily-mode'."
  (let ((inhibit-read-only t))
    (kill-all-local-variables)
    (setq major-mode 'mygtd-daily-mode
          mode-name "Mygtd Daily")
    (use-local-map mygtd-daily-mode-map)
    (erase-buffer)
    (buffer-disable-undo)))

(defun mygtd-daily-keymap-setup ()
  "Setup the keymap of `mygtd-daily-mode'."
  (setq mygtd-daily-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map "p" #'mygtd-daily-show-previous)
          (define-key map "n" #'mygtd-daily-show-next)
          (define-key map "d" #'mygtd-daily-task-finish)
          (define-key map "u" #'mygtd-daily-task-undo)
          map)))

(defun mygtd-daily-show (&optional date)
  "Show the view of mygtd-daily buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create mygtd-daily-buffer))
  (mygtd-daily-buffer-setup)
  (mygtd-daily-keymap-setup)
  (let* ((date (or date (format-time-string "%Y%m%d")))
         (ewoc (ewoc-create
                'mygtd-daily-pp
                (concat (propertize (concat "Mygtd Daily\n\n") 'face '(:height 1.5))
                        (propertize (concat (mygtd-time-to-str date) "\n")
                                    'face '(:height 1.1)))))
         (datas (mygtd-task-interval-query date)))
    (setq mygtd-daily-date date)
    (set (make-local-variable 'mygtd-daily-ewoc) ewoc)
    (if datas
        (dolist (data datas)
          (ewoc-enter-last ewoc data))
      (ewoc-enter-last ewoc nil)))
  (mygtd-mode 1)
  (read-only-mode 1))

(defun mygtd-daily-show-next ()
  "Switch to the view of next date"
  (interactive)
  (mygtd-daily-show
   (format-time-string "%Y%m%d" (+ (mygtd-date-to-second mygtd-daily-date)
                                   (* 24 60 60)))))

(defun mygtd-daily-show-previous ()
  "Switch to the view of previous date"
  (interactive)
  (mygtd-daily-show
   (format-time-string "%Y%m%d" (- (mygtd-date-to-second mygtd-daily-date)
                                   (* 24 60 60)))))

(defun mygtd-daily-refresh ()
  )

(defun mygtd-daily-task-finish (&optional task-id)
  "Update the status to done for task with TASK-ID or task at point."
  (interactive)
  (mygtd-ewoc-update :status "done")
  (let ((id (or task-id (mygtd-task-prop :id))))
    (mygtd-db-query `[:update task :set (= status "done") :where (= id ,id)])))

(defun mygtd-daily-task-undo (&optional task-id)
  "Update the status to todo for task with TASK-ID or task at point."
  (interactive)
  (mygtd-ewoc-update :status "todo")
  (let ((id (or task-id (mygtd-task-prop :id))))
    (mygtd-db-query `[:update task :set (= status "todo") :where (= id ,id)])))

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

