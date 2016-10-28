(defvar yxl-buffer-stored-name nil)

(defun yxl-buffer/store-name ()
  (interactive)
  (setq yxl-buffer-stored-name (buffer-name)))

(defun yxl-buffer/visit-stored-buffer ()
  (interactive)
  (set-window-buffer (selected-window) yxl-buffer-stored-name))

(provide 'yxl-buffer)
