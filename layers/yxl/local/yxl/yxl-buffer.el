(defvar yxl-buffer-stored-name nil)

(defun yxl-buffer/store-name ()
  (interactive)
  (setq yxl-buffer-stored-name (buffer-name))
  (message "stored buffer name: %s" yxl-buffer-stored-name))

(defun yxl-buffer/visit-stored-buffer ()
  (interactive)
  (set-window-buffer (selected-window) yxl-buffer-stored-name)
  (message "switch to stored buffer: %s" yxl-buffer-stored-name))

(provide 'yxl-buffer)
