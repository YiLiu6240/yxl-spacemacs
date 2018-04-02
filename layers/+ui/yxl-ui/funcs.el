(defun yxl-treemacs-visit-node-external ()
  (interactive)
  (let ((path (treemacs--prop-at-point :path)))
    (yxl-open-file-external path)))
