(defun yxl/custom-layout-1 ()
  "window layout 1 | 2/3"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left))

(defun yxl/custom-layout-2 ()
  "window layout 1/2 | 3/4"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left)
  (split-window-below))

(defun yxl/custom-layout-3 ()
  "window layout 1/2 | 3/4/5"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (split-window-below)
  (windmove-left))



(defun yxl/view-today-sidebar ()
  "show today task window, as a sidebar"
  (interactive)
  (split-window-right)
  (find-file yxl-file-org-main)
  (evil-window-set-width 30))
(defun yxl/view-todo-panel ()
  "open task windows: today, monthly, projects"
  (interactive)
  (yxl/custom-layout-1)
  (find-file yxl-file-org-main)
  (windmove-right)
  (cfw-open-calendar))



(defun yxl/split-window-below (&optional size)
  "split window with customized prefixes.
If C-u, create a window with 20 lines.
If C-u C-u, create a window with 15 lines.
Else create a window with lines parsed by prefix"
  (interactive "P")
  (cond
   ((equal size nil)
    (split-window-below))
   ((equal size '(4)) ; C-u
    (split-window-below -20))
   ((equal size '(16)) ; C-u C-u
    (split-window-below -15))
   (t (split-window-below size))))

(defun yxl/split-window-right (&optional size)
  "split window with customized prefixes.
If C-u, create a window with 20 lines.
If C-u C-u, create a window with 15 lines.
Else create a window with lines parsed by prefix"
  (interactive "P")
  (cond
   ((equal size nil)
    (split-window-right))
   ((equal size '(4)) ; C-u
    (split-window-right -20))
   ((equal size '(16)) ; C-u C-u
    (split-window-right -15))
   (t (split-window-right size))))

(defun yxl/split-window-below-and-focus (&optional size)
  "Split the window vertically and focus the new window."
  (interactive "P")
  (yxl/split-window-below size)
  (windmove-down))

(defun yxl/split-window-right-and-focus (&optional size)
  "Split the window vertically and focus the new window."
  (interactive "P")
  (yxl/split-window-right size)
  (windmove-right))

(defun split-window-below-small ()
  "split into a smaller window below, and move to focus"
  ;; TODO: create a variable for the "-20",
  ;; default to -20, if no numeric argument present
  (interactive)
  (if current-prefix-arg
      (list
       (yxl/split-window-below -20)
       (windmove-down)
       (spacemacs/new-empty-buffer))
    (list
     (yxl/split-window-below -20)
     (windmove-down))))

(defun split-window-right-small ()
  "split into a smaller window below, and move to focus"
  ;; TODO: create a variable for the "-20",
  ;; default to -20, if no numeric argument present
  (interactive)
  (if current-prefix-arg
      (list
        (yxl/split-window-right -35)
        (windmove-right)
        (spacemacs/new-empty-buffer))
    (list
     (yxl/split-window-right -35))))

(defun split-window-above-small ()
  "split into a smaller window above, and move to focus"
  (interactive)
  (if current-prefix-arg
      (list
       (yxl/split-window-below 20)
       (spacemacs/new-empty-buffer))
    (list
     (yxl/split-window-below 20))))

(defun split-window-left-small ()
  "split into a smaller window above, and move to focus"
  (interactive)
  (if current-prefix-arg
      (list
       (yxl/split-window-right 35)
       (windmove-right)
       (spacemacs/new-empty-buffer))
    (list
     (yxl/split-window-right 35)
     (windmove-right))))


(defvar-local yxl-line-width 80
  "preferred line width for a window, useful when setting window centering")

(defun yxl/change-window-width (width)
  (interactive "nwindow width: ")
  (evil-resize-window width t))

(defun yxl/center-window-margins ()
  "center current window"
  (interactive)
  (let* ((margin (max 0 (/ (- (window-width) yxl-line-width) 2))))
    (set-window-margins nil margin margin)))

(provide 'yxl-window)
