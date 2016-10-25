(defvar-local yxl-line-width 80
  "preferred line width for a window, useful when setting window centering")

(defun yxl-window/popwin-width ()
  "calculate the size of the popwin window, base on current frame width"
  (let ((auto-width (round (* (frame-width) 0.20))))
    (cond ((> (frame-width) 100)
           auto-width)
          (t
           20))))

(defun yxl-window/golden-ratio-width ()
  "get the width size of a window that should be expanded to gold ratio"
  (interactive)
  (let* ((golden-ratio 0.618)
         (main-width (round (* golden-ratio (frame-width)))))
    (evil-resize-window main-width t)))

(defun yxl-window/golden-ratio-height ()
  "get the height size of a window that should be expanded to gold ratio"
  (interactive)
  (let* ((golden-ratio 0.618)
         (main-height (round (* golden-ratio (frame-height)))))
    (evil-resize-window main-height nil)))


(defun yxl-window/custom-layout1 ()
  "window layout 1 | 2/3"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left))

(defun yxl-window/custom-layout2 ()
  "window layout 1/2 | 3/4"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left)
  (split-window-below))

(defun yxl-window/custom-layout3 ()
  "window layout 1/2 | 3/4/5"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (split-window-below)
  (windmove-left))



(defun yxl-window/split-below (&optional size)
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

(defun yxl-window/split-right (&optional size)
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

(defun yxl-window/split-below-and-focus (&optional size)
  "Split the window vertically and focus the new window."
  (interactive "P")
  (yxl-window/split-below size)
  (windmove-down))

(defun yxl-window/split-right-and-focus (&optional size)
  "Split the window vertically and focus the new window."
  (interactive "P")
  (yxl-window/split-right size)
  (windmove-right))

(defun split-window-below-small ()
  "split into a smaller window below, and move to focus"
  ;; TODO: create a variable for the "-20",
  ;; default to -20, if no numeric argument present
  (interactive)
  (if current-prefix-arg
      (list
       (yxl-window/split-below -20)
       (windmove-down)
       (spacemacs/new-empty-buffer))
    (list
     (yxl-window/split-below -20)
     (windmove-down))))

(defun split-window-right-small ()
  "split into a smaller window below, and move to focus"
  ;; TODO: create a variable for the "-20",
  ;; default to -20, if no numeric argument present
  (interactive)
  (if current-prefix-arg
      (list
        (yxl-window/split-right -35)
        (windmove-right)
        (spacemacs/new-empty-buffer))
    (list
     (yxl-window/split-right -35))))

(defun split-window-above-small ()
  "split into a smaller window above, and move to focus"
  (interactive)
  (if current-prefix-arg
      (list
       (yxl-window/split-below 20)
       (spacemacs/new-empty-buffer))
    (list
     (yxl-window/split-below 20))))

(defun split-window-left-small ()
  "split into a smaller window above, and move to focus"
  (interactive)
  (if current-prefix-arg
      (list
       (yxl-window/split-right 35)
       (windmove-right)
       (spacemacs/new-empty-buffer))
    (list
     (yxl-window/split-right 35)
     (windmove-right))))


(defun yxl-window/change-width (width)
  (interactive "nwindow width: ")
  (evil-resize-window width t))

(defun yxl-window/center-margins ()
  "center current window"
  (interactive)
  (let* ((margin (max 0 (/ (- (window-width) yxl-line-width) 2))))
    (set-window-margins nil margin margin)))


(defun yxl-window/get-buffer-previous-window ()
  "open in the current window the buffer in previous window."
  (interactive)
  (set-window-buffer (selected-window) (window-buffer (previous-window))))

(provide 'yxl-window)
