;;; mygtd-macs.el --- Utilities for mygtd functions.  -*- lexical-binding: t; -*-

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

(defun mygtd-ewoc-node ()
  (ewoc-locate mygtd-daily-ewoc))

(defun mygtd-ewoc-data ()
  "Return the ewoc data at point."
  (ewoc-data (mygtd-ewoc-node)))

(defun mygtd-ewoc-update (prop val)
  (let ((node (mygtd-ewoc-node)))
    (ewoc-set-data node (plist-put (mygtd-ewoc-data) prop val))
    (ewoc-invalidate mygtd-daily-ewoc node)))

(defun mygtd-task-prop (prop)
  "Return the value of PROP for task at point."
  (plist-get (mygtd-ewoc-data) prop))

(defun plist->alist (plist)
  "Convert a plist to a alist."
  (if (null plist)
      '()
    (cons
     (cons (car plist) (cadr plist))
     (plist->alist (cddr plist)))))

(defvar mygtd-year-fmt "%Y")

(defvar mygtd-month-fmt "%Y-%m")

(defvar mygtd-date-fmt "%Y-%m-%d")

(defun mygtd-year-shown (mygtd-year)
  (format-time-string mygtd-year-fmt
                      (mygtd-date-to-time (format "%s0101" mygtd-year))))

(defun mygtd-month-shown (mygtd-month)
  (format-time-string mygtd-month-fmt
                      (mygtd-date-to-time (format "%s%s01" (substring mygtd-month 0 4)
                                                  (substring mygtd-month 4 6)))))

(defun mygtd-date-shown (mygtd-date)
  (format-time-string mygtd-date-fmt (mygtd-date-to-time mygtd-date)))

(mygtd-date-shown "20221010" "%Y-%d-%m")

(defun mygtd-month-to-org-date (month)
  (format "%s-%s-%s"
          (substring date 0 4)
          (substring date 4 6)
          (substring date 6)))

(defun mygtd-date-to-org-date (date)
  (format "%s-%s-%s"
          (substring date 0 4)
          (substring date 4 6)
          (substring date 6)))

(defun mygtd-date-to-full-org-date (date)
  (format "%s-%s-%s 00:00:00"
          (substring date 0 4)
          (substring date 4 6)
          (substring date 6)))

(defun mygtd-date-to-time (date)
  "Convert mygtd date to second."
  (date-to-time (mygtd-date-to-full-org-date date)))

(defun mygtd-date-to-second (date)
  "Convert mygtd date to second."
  (time-to-seconds (mygtd-date-to-time date)))

;; (mygtd-date-to-org-date "20221010")

(defun mygtd-time-to-str (time)
  "Convert the mygtd format of time to meaningful time string.
The 'mygtd format of time' are like 20220916, 202209, 2022 etc."
  (pcase (length time)
    (8 (format "%s-%s-%s"
               (substring time 0 4)
               (substring time 4 6)
               (substring time 6)))
    (6 (format "%s-%s"
               (substring time 0 4)
               (substring time 4 6)))
    (4 (format "%s"
               (substring time 0 4)))
    (_ (error "Invalid format of mygtd time!"))))

(defun mygtd-next-date (date num)
  (format-time-string "%Y%m%d" (+ (mygtd-date-to-second mygtd-daily-date)
                                  (* 24 60 60))))

(defun mygtd-time-less-p (time1 time2)
  "Judge if TIME1 is less than TIME2."
  (if (= (length time1) (length time2))
      (pcase (length time1)
        (8 (time-less-p (mygtd-date-to-second time1) (mygtd-date-to-second time2)))
        (6 (< (string-to-number (substring time1 4 6))
              (string-to-number (substring time2 4 6))))
        (4 (< (string-to-number time1) (string-to-number time2))))
    (error "Invalid mygtd time number comparation: %s, %s" time1 time2)))

(defun mygtd-time-wider-p (time1 time2)
  "Judge if TIME1 is wider than TIME2."
  (if (= (length time1) (length time2))
      (error "Invalid mygtd time wide comparation: %s, %s" time1 time2)
    (< (length time1) (length time2))))

(defun mygtd-query-wrapper (table query-result)
  "Convert the db QUERY-RESULT to a list of plist.
TABLE is the table name."
  (let ((kwd-lst (mapcar (lambda (el)
                           (intern (concat ":" (symbol-name (car el)))))
                         (cadr (assoc table mygtd-db--table-schemata)))))
    (mapcar (lambda (data)
              (let ((res))
                ;; FIXME: replace with a built-in function if exists.
                (dotimes (i (length data))
                  (setq res (append res (list (nth i kwd-lst) (nth i data)))))
                res))
            query-result)))

(defvar mygtd-window-margin 3
  "Window margins of mygtd buffer window.")

(defun mygtd-buffer-p ()
  "Check if current buffer belongs to mygtd."
  (or (string= (buffer-name) mygtd-daily-buffer)))

(defun mygtd-preserve-window-margin ()
  "Preserve window margins of mygtd buffer."
  (save-selected-window
    (dolist (win (window-list))
      (select-window win)
      (when (and (mygtd-buffer-p) mygtd-window-margin)
        (set-window-margins (selected-window)
                            mygtd-window-margin
                            mygtd-window-margin)))))

;; (defun mygtd-buf-setup (pp header footer)
;;   "Setup of mygtd buffer."
;;   (let ((ewoc (ewoc-create pp header footer t))
;;         (inhibit-read-only t))
;;     (erase-buffer)
;;     (kill-all-local-variables)
;;     (remove-overlays)
;;     (buffer-disable-undo)
;;     ;; (setq-local cursor-type nil)
;;     (set (make-local-variable mygtd-ewoc) ewoc)))

;; (defconst mygtd-status-todo "todo"
;;   "String that represents 'todo' status.")

;; (defvar mygtd-status-todo-icon "▢"
;;   "Icon that represents 'todo' status.")

;; (defconst mygtd-status-done "done"
;;   "String that represents 'done' status.")

;; (defvar mygtd-status-done-icon "✓"
;;   "Icon that represents 'done' status.") 

;; (defun mygtd-status-icon (status)
;;   (pcase status
;;     ((pred #'string= mygtd-status-todo) mygtd-status-todo-icon)
;;     ((pred #'string= mygtd-status-done) mygtd-status-done-icon)))

(provide 'mygtd-macs)
