(defvar yxl-org-agenda-files-life (list yxl-file-org-main
                                        yxl-file-org-config))
(defvar yxl-org-agenda-files-work
  (list yxl-file-org-scratch
        yxl-file-org-work
        (concat yxl-path-org "tasks/" "proj_ctw.org")
        (concat yxl-path-org "tasks/" "proj_bham.org")))
(defvar yxl-org-agenda-files (append yxl-org-agenda-files-life
                                     yxl-org-agenda-files-work))
