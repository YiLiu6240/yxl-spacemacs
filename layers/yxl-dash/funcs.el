(defun yxl-dash//activate-package-docsets (path)
  "Add dash docsets from specified PATH."
  (setq helm-dash-docsets-path (expand-file-name path))
  (setq helm-dash-common-docsets (helm-dash-installed-docsets))
  (message (format "activated %d docsets from: %s"
                   (length helm-dash-common-docsets) path)))

(defun yxl-dash/select-docset ()
  (interactive)
  (let* ((counsel-dash-common-docsets
          (list (ivy-read "which docset to use: "
                          (helm-dash-installed-docsets)))))
    (counsel-dash)))
