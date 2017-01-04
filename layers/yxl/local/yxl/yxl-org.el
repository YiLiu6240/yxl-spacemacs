(define-minor-mode yxl-org-task-mode
  "Features for org buffers that are essentially todo list."
  :lighter ""
  (if yxl-org-task-mode
      (let ((scale 0.5))
        (text-scale-decrease scale)
        (org-indent-mode 1))
    (text-scale-set 0)))

(defun yxl-org-task-file-p (&optional file)
  "Return non-nil, if FILE is in `yxl-org-task-files'.
If FILE is omitted, use the file associated with the current
buffer."
  (let ((fname (or file (buffer-file-name))))
    (and fname
         (member (file-truename fname)
                 (mapcar #'file-truename yxl-org-task-files)))))

(defun yxl-org-format-task-files ()
  "If the current file is in `org-agenda-files',
then turn on `yxl-org-task-mode'"
  (if (yxl-org-task-file-p)
      (yxl-org-task-mode 1)))

(defun yxl-org-open-all-task-files ()
  (interactive)
  (yxl-find-file-open-all yxl-org-task-files))

(provide 'yxl-org)
