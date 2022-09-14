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


(defun plist->alist (plist)
  "Convert a plist to a alist."
  (if (null plist)
      '()
    (cons
     (cons (car plist) (cadr plist))
     (plist->alist (cddr plist)))))






(defvar mygtd-window-margin 3
  "Window margins of mygtd buffer window.")

(defun mygtd-buf-setup (pp header footer)
  "Setup of mygtd buffer."
  (let ((ewoc (ewoc-create pp header footer t))
        (inhibit-read-only t))
    (erase-buffer)
    (kill-all-local-variables)
    (remove-overlays)
    (buffer-disable-undo)
    ;; (setq-local cursor-type nil)
    (set (make-local-variable mygtd-ewoc) ewoc)))

(defun mygtd-buffer-p ()
  "Check if current buffer belongs to mygtd."
  (or (string= (buffer-name) mygtd-daily-buf)))

(defun mygtd-preserve-window-margin ()
  "Preserve window margins of mygtd buffer."
  (save-selected-window
    (dolist (win (window-list))
      (select-window win)
      (when (and (mygtd-buffer-p) mygtd-window-margin)
        (set-window-margins (selected-window)
                            mygtd-window-margin
                            mygtd-window-margin)))))

(defconst mygtd-status-todo "todo"
  "String that represents 'todo' status.")

(defvar mygtd-status-todo-icon "▢"
  "Icon that represents 'todo' status.")

(defconst mygtd-status-done "done"
  "String that represents 'done' status.")

(defvar mygtd-status-done-icon "✓"
  "Icon that represents 'done' status.") 

(defun mygtd-status-icon (status)
  (pcase status
    ((pred #'string= mygtd-status-todo) mygtd-status-todo-icon)
    ((pred #'string= mygtd-status-done) mygtd-status-done-icon)))

(provide 'mygtd-macs)
