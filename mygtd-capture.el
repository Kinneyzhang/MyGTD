;;; capture task

(defvar mygtd-capture-mode (let ((map (make-sparse-keymap)))
                             (define-key map "\C-c\C-d" 'mygtd-capture-finish)
                             (define-key map "\C-c\C-k" 'mygtd-capture-cancel)
                             map))

(defun mygtd-capture-data-diff ()
  )

(defun mygtd-capture-finish ()
  (interactive)
  (let ((new-data ))
    (mygtd-daily-ewoc-data-update)))

(defun mygtd-capture-cancel ()
  (interactive)
  (kill-buffer mygtd-capture-buffer))

(defvar mygtd-daily-old-data nil
  "The old ewoc buffer data before editing.")

(defvar mygtd-daily-new-data nil
  "The new ewoc buffer data after editing.")

(defvar mygtd-capture-buffer "*Mygtd Capture*")

(defun mygtd-capture ()
  "Add mygtd tasks in a capture buffer."
  (interactive)
  (let ((old-data mygtd-daily-ewoc-data))
    (switch-to-buffer mygtd-capture-buffer)
    (setq major-mode 'mygtd-capture-mode)
    (setq mode-name "mygtd-capture")
    (setq-local header-line-format
                (substitute-command-keys "\\<mygtd-capture-mode-map>Capture Tasks: `\\[mygtd-capture-finish]' to finish, `\\[mygtd-capture-cancel]' to cancel"))
    (use-local-map mygtd-capture-mode-map)
    (dolist (data old-data)
      (let ((name (plist-get data :name))
            (status (plist-get data :status)))
        (insert "- " (mygtd-task-status-text status) " " name "\n")))
    (use-local-map org-mode-map)
    (mygtd-prettify-mode 1)))
