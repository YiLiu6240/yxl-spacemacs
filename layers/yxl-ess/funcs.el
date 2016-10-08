(defun yxl-ess/ess-repl-2cols ()
  (interactive)
  (delete-other-windows)

  ;; create two cols
  (split-window-right)

  ;; create an empty R mode buffer
  (split-window-below -15)
  (windmove-down)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)
    (R-mode))
  (windmove-up)

  ;; create ess process and start a helper buffer
  (windmove-right)
  (split-window-below 20)
  (windmove-down)
  (call-interactively 'R)
  (split-window-below -20)
  (ess-help "ls")

  ;; restore focus to main window
  (windmove-left)
  (windmove-up))

(defun yxl-ess/ess-repl-3cols ()
  (interactive)
  (delete-other-windows)

  ;; create three cols
  (split-window-right)
  (split-window-right)

  ;; create an empty R mode buffer
  (split-window-below -20)
  (windmove-down)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)
    (R-mode))
  (windmove-up)

  ;; create a middle pane
  (windmove-right)
  (split-window-below -20)

  ;; create ess process and start a helper buffer
  (windmove-right)
  (call-interactively 'R)
  (split-window-below -20)
  (ess-help "ls")

  ;; restore focus to main window
  (windmove-left)
  (windmove-left))
