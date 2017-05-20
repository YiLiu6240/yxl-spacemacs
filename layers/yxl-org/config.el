;; TODO: find a better way to organize these
(defvar yxl-org-agenda-files-life (list yxl-file-org-main
                                        yxl-file-org-config))
(defvar yxl-org-agenda-files-work
  (list yxl-file-org-scratch
        yxl-file-org-work
        (concat yxl-path-org "tasks/" "proj_ctw.org")
        (concat yxl-path-org "tasks/" "proj_bham.org")))

(defvar yxl-org-agenda-commands
  '(
    ("0" "Quick Todo List"
     ((agenda "Agenda" ((org-agenda-ndays 7)
                        (org-agenda-start-day "-2d")))
      (alltodo "" ((org-agenda-files (list yxl-file-org-scratch))))
      (todo "TODAY|TODO"
            ((org-agenda-files yxl-org-agenda-files-life)))
      (todo "INBOX|QUICK|HAVE-A-LOOK"
            ((org-agenda-files yxl-org-agenda-files-life)))
      (todo "NEXT"
            ((org-agenda-files yxl-org-agenda-files-life)))))
    ("9" "Life -- Bi-Weekly"
     ((agenda "Agenda" ((org-agenda-ndays 14)
                        (org-agenda-start-day "-7d")
                        (org-agenda-files yxl-org-agenda-files-life)
                        (org-agenda-repeating-timestamp-show-all t)))
      (todo "TODAY"
            ((org-agenda-files yxl-org-agenda-files-life)))
      (todo "INBOX|QUICK|HAVE-A-LOOK"
            ((org-agenda-files yxl-org-agenda-files-life)))
      (todo "TODO|NEXT"
            ((org-agenda-files yxl-org-agenda-files-life)))
      (todo "FOLLOW-UP|SOMEDAY"
            ((org-agenda-files yxl-org-agenda-files-life)))))
    ("1" "Work -- todo list"
     ((todo "TODAY"
            ((org-agenda-files yxl-org-agenda-files-work)))
      (todo "INBOX|QUICK|HAVE-A-LOOK"
            ((org-agenda-files yxl-org-agenda-files-work)))
      (todo "TODO|NEXT"
            ((org-agenda-files yxl-org-agenda-files-work)))
      (todo "FOLLOW-UP|SOMEDAY"
            ((org-agenda-files yxl-org-agenda-files-work)))))
    ("2" "Work -- 14 Days"
     ((agenda "Agenda" ((org-agenda-ndays 14)
                        (org-agenda-start-day "-7d")
                        (org-agenda-files yxl-org-agenda-files-work)
                        (org-agenda-repeating-timestamp-show-all t)))
      (todo "TODAY"
            ((org-agenda-files yxl-org-agenda-files-work)))
      (todo "INBOX|QUICK|HAVE-A-LOOK"
            ((org-agenda-files yxl-org-agenda-files-work)))
      (todo "TODO|NEXT"
            ((org-agenda-files yxl-org-agenda-files-work)))
      (todo "FOLLOW-UP|SOMEDAY"
            ((org-agenda-files yxl-org-agenda-files-work)))))
    ("3" "Work -- 30 Days"
     ((agenda "Agenda" ((org-agenda-ndays 30)
                        (org-agenda-start-day "-7d")
                        (org-agenda-files yxl-org-agenda-files-work)
                        (org-agenda-repeating-timestamp-show-all t)))
      (todo "TODAY"
            ((org-agenda-files yxl-org-agenda-files-work)))
      (todo "INBOX|QUICK|HAVE-A-LOOK"
            ((org-agenda-files yxl-org-agenda-files-work)))
      (todo "TODO|NEXT"
            ((org-agenda-files yxl-org-agenda-files-work)))
      (todo "FOLLOW-UP|SOMEDAY"
            ((org-agenda-files yxl-org-agenda-files-work)))))))
