(defvar-local yxl-line-width 80
  "preferred line width for a window, useful when setting window centering")

(defvar yxl-window-stored-layout nil)


(defun yxl-window-popwin-width ()
  "calculate the size of the popwin window, base on current frame width"
  (let ((auto-width (round (* (frame-width) 0.20))))
    (cond ((> (frame-width) 100)
           auto-width)
          (t
           20))))

(defun yxl-window-get-ratio ()
  "Return the a window width/height corresponding to a ratio to current frame:
- golden ratio by default;
- 0.8 with C-u uni prefix;
- 0.X with uni prefix being single digit;
- X% otherwise."
  "By default return a golden ratio column width; if has uni prefix, return 0.8;
if uni prefix is a one digit number, treat it as 0.X; else treat it as X%."
  (let* ((golden-ratio 0.618))
    (cond
     ((equal current-prefix-arg nil)
      golden-ratio)
     ((equal current-prefix-arg  '(4))
      0.75)
     (t (cond
         ((< current-prefix-arg 10)(/ current-prefix-arg 10.0))
         (t (/ current-prefix-arg 100.0)))))))

(defun yxl-window-adjust-width-ratio ()
  "get the width size of a window that should be expanded to gold ratio"
  (interactive)
  (let* ((main-width (round (* (yxl-window-get-ratio) (frame-width)))))
    (evil-resize-window main-width t)))

(defun yxl-window-adjust-height-ratio ()
  "get the height size of a window that should be expanded to gold ratio"
  (interactive)
  (let* ((main-height (round (* (yxl-window-get-ratio) (frame-height)))))
    (evil-resize-window main-height nil)))


(defun yxl-window-custom-layout1 ()
  "window layout 1 | 2/3"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left))

(defun yxl-window-custom-layout2 ()
  "window layout 1/2 | 3/4"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left)
  (split-window-below))

(defun yxl-window-custom-layout3 ()
  "window layout 1/2 | 3/4/5"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (split-window-below)
  (windmove-left))



(defun yxl-window-split-horizontal-focus ()
  (interactive)
  (cond
   ((equal current-prefix-arg '(4))
    (progn
      (split-window-below 20)))
   (t
    (progn
      (split-window-below -20)
      (evil-window-down 1)))))

(defun yxl-window-split-horizontal-stay ()
  (interactive)
  (cond
   ((equal current-prefix-arg '(4))
    (progn
      (split-window-below 20)
      (evil-window-down 1)))
   (t
    (progn
      (split-window-below -20)))))

(defun yxl-window-split-vertical-focus ()
  (interactive)
  (cond
   ((equal current-prefix-arg '(4))
    (progn
      (split-window-right 20)
      (evil-window-left 1)))
   (t
    (progn
      (split-window-right -20)
      (evil-window-right 1)))))

(defun yxl-window-split-vertical-stay ()
  (interactive)
  (cond
   ((equal current-prefix-arg '(4))
    (progn
      (split-window-right 20)
      (evil-window-right 1)))
   (t
    (progn
      (split-window-right -20)
      (evil-window-left 1)))))


(defun yxl-window-change-width (width)
  (interactive "nwindow width: ")
  (evil-resize-window width t))

(defun yxl-window-center-margins ()
  "center current window"
  (interactive)
  (let* ((margin (max 0 (/ (- (window-width) yxl-line-width) 2))))
    (set-window-margins nil margin margin)))


(defun yxl-window-get-buffer-previous-window ()
  "open in the current window the buffer in previous window."
  (interactive)
  (set-window-buffer (selected-window) (window-buffer (previous-window))))

(defun yxl-window-record-layout ()
  (interactive)
  (progn
    (setq yxl-window-stored-layout (window-state-get nil t))
    (message "stored window layout.")))

(defun yxl-window-load-laytout ()
  (interactive)
  (progn
    (window-state-put yxl-window-stored-layout (frame-root-window))
    (message "load stored window layout.")))

(provide 'yxl-window)
