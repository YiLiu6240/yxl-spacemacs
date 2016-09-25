(defun yxl/org-agenda-work ()
  (interactive)
  (org-agenda nil "1"))
(defun yxl/org-agenda-life ()
  (interactive)
  (org-agenda nil "0"))

(defun yxl-org/setup-general ()
  (setq org-directory 'yxl/org-directory)
  ;; disable middle split
  (setq org-M-RET-may-split-line nil)
  ;; org title bullets
  ;; (setq org-bullets-bullet-list '("*" "+" ">" "-"))
  ;; Make the org calendar start on monday
  (setq calendar-week-start-day 1)
  ;; disable org folding at launch
  (setq org-startup-folded nil)
  ;; disable truncate-lines at launch
  (setq org-startup-truncated nil)
  (setq org-startup-indented t)
  ;; highlight code-block
  (setq org-src-fontify-natively t)
  ;; do not add timestamp when closing todos
  (setq org-log-done nil)
  ;; start display tags after col 60
  (setq org-tags-column 0)
  ;; (setq org-fast-tag-selection-single-key t)
  (setq org-insert-heading-respect-content t))

(defun yxl-org/setup-capture ()
  (setq org-capture-templates
        '(("i" "life-inbox" entry
           (file+headline yxl/org-file-master "Inbox")
           "* INBOX %?\n  %i\n")
          ("t" "life-todo" entry
           (file+headline yxl/org-file-master "Inbox")
           "* TODO %?\n  %i\n")
          ("c" "config" entry
           (file+headline yxl/org-file-master "Config")
           "* INBOX %?\n  %i\n")
          ("n" "quick note" item
           (file+headline yxl/file-note-master "Quick Notes"))
          ("I" "work-inbox" entry
           (file+headline yxl/org-file-work "Inbox")
           "* INBOX %?\n  %i\n")
          ("T" "work-todo" entry
           (file+headline yxl/org-file-work "Inbox")
           "* TODO %?\n  %i\n")
          ("l" "logs" entry
           (file+datetree yxl/org-log-file)
           "* %?\n  -- %U\n  %i\n"))))

(defun yxl-org/setup-keywords ()
  (setq org-todo-keywords
        '((sequence
           "INBOX(i)"                   ;; ideas, undecided
           "QUICK(q)"                   ;; quick
           "HAVE-A-LOOK(h)"
           "TODO(t)"                        ;; needs to be done
           "NEXT(n)"                        ;; next in line
           "DOING(I)"                       ;; in-progress
           "HOLD(H)"                        ;; put on hold for various reasons
           "FOLLOW-UP(f)"                   ;; follow-up results
           "SOMEDAY(s)"                     ;; not now
           "|"
           "DONE(d)" "CANCELED(C)" "FAILED(F)")
          (sequence
           "00(0)" "25(1)" "50(2)" "75(3)" "95(4)"
           "|"
           "DONE(d) ABORT(A)")))
  (setq org-todo-keyword-faces
        '(("INBOX" . (:foreground "#268bd2" :weight bold))
          ("HAVE-A-LOOK" . (:foreground "#d33682" :weight bold ))
          ("TODO" . (:foreground "#cb4b16" :weight bold ))
          ("NEXT" . (:foreground "#6c71c4" :weight bold))
          ("DOING" . (:foreground "#b58900" :weight bold))
          ("00" . (:foreground "#deab0e" :weight bold))
          ("25" . (:foreground "#b58900" :weight bold))
          ("50" . (:foreground "#b58900" :weight bold))
          ("75" . (:foreground "#926e00" :weight bold))
          ("95" . (:foreground "#926e00" :weight bold))))
  (setq org-tag-faces
        '(("CAPTURE" . (:foreground "#268bd2" :slant italic))
          ("CTW" . (:foreground "#268bd2" :slant italic))
          ("WORK" . (:foreground "#268bd2" :slant italic))
          ("HOME" . (:foreground "#859900" :slant italic))
          ("CONFIG" . (:foreground "#859900" :slant italic))
          ("MAJOR" . (:foreground "#cb4b16" :slant italic))
          ("MID" . (:foreground "#b58900" :slant italic))
          ("MINOR" . (:foreground "#859900" :slant italic))))
  (setq org-tag-persistent-alist
        '((:startgrouptag) ;; task group
          ("CTW" . ?t) ("WORK" . ?w) ("HOME" . ?h) ("CONFIG" . ?c)
          (:endgrouptag)
          (:startgrouptag) ;; effort required
          ("MAJOR" . ?1) ("MID" . ?2) ("MINOR" . ?3)
          (:endgrouptag)
          (:startgrouptag) ;; actions to take
          ("ISSUES") ("HAVE_A_LOOK") ("THINK") ("REFACTOR")
          (:endgrouptag)
          (:startgrouptag) ;; task propertiy
          ("URGENT" . ?u) ("KEY" . ?k) ("HARD" . ?a) ("BONUS" . ?b)
          (:endgrouptag))))

(defun yxl-org/setup-agenda ()
  ;; agenda file
  (setq org-agenda-files yxl/org-agenda-files)
  ;; agenda view: 1 month
  (setq org-agenda-span 'month)
  ;; org agenda time grid
  (setq org-agenda-time-grid '((daily today)
                               "----------------"
                               (0900 1100 1300 1500 1700)))

  (add-to-list 'org-agenda-custom-commands
               '("0" "Life -- Weekly"
                 ((agenda "Agenda" ((org-agenda-ndays 7)
                                    (org-agenda-files yxl/org-agenda-files-life)
                                    (org-agenda-repeating-timestamp-show-all t)))
                  (todo "INBOX|QUICK|HAVE-A-LOOK" ((org-agenda-files yxl/org-agenda-files-life)))
                  (todo "TODO|NEXT" ((org-agenda-files yxl/org-agenda-files-life)))
                  (todo "DOING|00|25|50|75|95" ((org-agenda-files yxl/org-agenda-files-life)))
                  (todo "FOLLOW-UP|SOMEDAY" ((org-agenda-files yxl/org-agenda-files-life)))))
               t)
  (add-to-list 'org-agenda-custom-commands
               '("9" "Life -- Bi-Weekly"
                 ((agenda "Agenda" ((org-agenda-ndays 14)
                                    (org-agenda-start-day "-7d")
                                    (org-agenda-files yxl/org-agenda-files-life)
                                    (org-agenda-repeating-timestamp-show-all t)))
                  (todo "INBOX|QUICK|HAVE-A-LOOK" ((org-agenda-files yxl/org-agenda-files-life)))
                  (todo "TODO|NEXT" ((org-agenda-files yxl/org-agenda-files-life)))
                  (todo "DOING|00|25|50|75|95" ((org-agenda-files yxl/org-agenda-files-life)))
                  (todo "FOLLOW-UP|SOMEDAY" ((org-agenda-files yxl/org-agenda-files-life)))))
               t)

  (add-to-list 'org-agenda-custom-commands
               '("1" "Work -- 7 Days"
                 ((agenda "Agenda" ((org-agenda-ndays 7)
                                    (org-agenda-files yxl/org-agenda-files-work)
                                    (org-agenda-repeating-timestamp-show-all t)))
                  (todo "INBOX|QUICK|HAVE-A-LOOK" ((org-agenda-files yxl/org-agenda-files-work)))
                  (todo "TODO|NEXT" ((org-agenda-files yxl/org-agenda-files-work)))
                  (todo "DOING|00|25|50|75|95" ((org-agenda-files yxl/org-agenda-files-work)))
                  (todo "FOLLOW-UP|SOMEDAY" ((org-agenda-files yxl/org-agenda-files-work)))))
               t)
  (add-to-list 'org-agenda-custom-commands
               '("2" "Work -- 14 Days"
                 ((agenda "Agenda" ((org-agenda-ndays 14)
                                    (org-agenda-start-day "-7d")
                                    (org-agenda-files yxl/org-agenda-files-work)
                                    (org-agenda-repeating-timestamp-show-all t)))
                  (todo "INBOX|QUICK|HAVE-A-LOOK" ((org-agenda-files yxl/org-agenda-files-work)))
                  (todo "TODO|NEXT" ((org-agenda-files yxl/org-agenda-files-work)))
                  (todo "DOING|00|25|50|75|95" ((org-agenda-files yxl/org-agenda-files-work)))
                  (todo "FOLLOW-UP|SOMEDAY" ((org-agenda-files yxl/org-agenda-files-work)))))
               t)
  (add-to-list 'org-agenda-custom-commands
               '("3" "Work -- 30 Days"
                 ((agenda "Agenda" ((org-agenda-ndays 30)
                                    (org-agenda-start-day "-7d")
                                    (org-agenda-files yxl/org-agenda-files-work)
                                    (org-agenda-repeating-timestamp-show-all t)))
                  (todo "INBOX|QUICK|HAVE-A-LOOK" ((org-agenda-files yxl/org-agenda-files-work)))
                  (todo "TODO|NEXT" ((org-agenda-files yxl/org-agenda-files-work)))
                  (todo "DOING|00|25|50|75|95" ((org-agenda-files yxl/org-agenda-files-work)))
                  (todo "FOLLOW-UP|SOMEDAY" ((org-agenda-files yxl/org-agenda-files-work)))))
               t))
