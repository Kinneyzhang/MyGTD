(defvar mygtd-task-status '("todo" "done"))

(defvar mygtd-task-periods '("morning" "afternoon" "evening"))

(defvar mygtd-task-categories '("work" "study" "think" "read"))

(defvar mygtd-task-locations '("house" "office" "outside"))

(defvar mygtd-task-devices '("PC" "mbp" "iPhone" "Kindle"))

(defun mygtd-task-twidget ()
  (with-twidget-setup
   (twidget-insert (propertize "Add a new task\n\n" 'face '(:height 1.2)))
   (twidget-create 'twidget-text
     :bind 'mygtd-task-name
     :format "Name: [t]")
   (twidget-insert "\n")
   (twidget-create 'twidget-choice
     :bind 'mygtd-task-category
     :format "Category: [t]"
     :choices mygtd-task-categories
     :require t)
   (twidget-insert "\n")
   (twidget-create 'twidget-choice
     :bind 'mygtd-task-state
     :format "Status: [t]"
     :choices mygtd-task-status
     :value mygtd-task-default-status
     :require t)
   (twidget-insert "\n")
   (twidget-create 'twidget-choice
     :bind 'mygtd-task-period
     :format "Period: [t]"
     :choices mygtd-task-periods)
   (twidget-insert "\n")
   (twidget-create 'twidget-choice
     :bind 'mygtd-task-location
     :format "Location: [t]"
     :choices mygtd-task-locations)
   (twidget-insert "\n")
   (twidget-create 'twidget-choice
     :bind 'mygtd-task-device
     :format "Device: [t]"
     :choices mygtd-task-devices)))

(defvar mygtd-twidget-buffer "Mygtd Twidget")

(defun mygtd-task-new ()
  "Create a new task."
  (interactive)
  (let ((buf (get-buffer-create mygtd-twidget-buffer)))
    (pop-to-buffer buf)
    (let ((inhibit-read-only t))
      (set-window-margins (selected-window) 4 4)
      (erase-buffer)
      (mygtd-task-twidget))))

(defun mygtd-task-edit (id)
  "Create a task with task ID."
  (interactive)
  )
