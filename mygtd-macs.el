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
