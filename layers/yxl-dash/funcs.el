(defun yxl-dash//activate-package-docsets (path)
  "Add dash docsets from specified PATH."
  (setq helm-dash-docsets-path (expand-file-name path))
  (setq helm-dash-common-docsets (helm-dash-installed-docsets))
  (message (format "activated %d docsets from: %s"
                   (length helm-dash-common-docsets) path)))

(defun yxl-dash/search-docset ()
  (interactive)
  (let* ((counsel-dash-common-docsets
          (list (ivy-read "which docset to use: "
                          (helm-dash-installed-docsets)))))
    (counsel-dash)))

(defun yxl-dash/search-docset-external-browser ()
  (interactive)
  (let ((counsel-dash-browser-func 'browse-url-generic))
    (yxl-dash/search-docset)))
