(defun yxl-dash-search-docset-firefox ()
  (interactive)
  (let ((counsel-dash-browser-func #'browse-url-firefox))
    (yxl-dash-search-docset)))

(defun yxl-dash-search-docset-chromium ()
  (interactive)
  (let ((counsel-dash-browser-func #'browse-url-chromium))
    (yxl-dash-search-docset)))

(defun yxl-doc-portal-chromium ()
  (interactive)
  (let ((browse-url-browser-function #'browse-url-chromium))
    (yxl-doc-portal)))

(defun yxl-dash-search-docset-helm ()
  "Use helm to select docstring first, becasue ivy uses mini-buffer
and in some situtations it will cause problems."
  (interactive)
  (helm-autoresize-mode t)
  (let* ((helm-autoresize-max-height 80)
         (helm-dash-docsets-path yxl-docs-docset-path)
         (helm-dash-common-docsets
          (list (helm :sources (helm-build-sync-source "docset"
                                 :candidates (helm-dash-installed-docsets))
                      :prompt "which docset to use: "
                      :buffer "*helm-dash-choose-docset"))))
    ;; Nevertheless helm is slow AF, so for the proper part, still use counsel
    (counsel-dash)))
