;; TODO: associate `yxl-org-task-mode' to the files in org agenda files
;; write a function to add hook to orgmode to check if
;; current file is in agenda files
;; if so, enable `yxl-org-task-mode'
(define-minor-mode yxl-org-task-mode
  "Features for org buffers that are essentially todo list."
  :lighter ""
  (if yxl-org-task-mode
      (let ((scale 0.5))
        (text-scale-decrease scale))
    (text-scale-set 0)))

(provide 'yxl-org)
