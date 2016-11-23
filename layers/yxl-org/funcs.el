(defun yxl-org/agenda-work ()
  (interactive)
  (org-agenda nil "1"))

(defun yxl-org/agenda-life ()
  (interactive)
  (org-agenda nil "0"))

(defun yxl-org/agenda-view ()
  (interactive)
  (delete-other-windows)
  (split-window-sensibly)
  (cfw-open-calendar)
  (call-interactively 'other-window)
  (org-todo-list))

(defun yxl-org/setup-general ()
  (setq org-directory 'yxl-path-org)
  ;; disable middle split
  (setq org-M-RET-may-split-line nil)
  ;; org title bullets
  ;; (setq org-bullets-bullet-list '("○"))
  ;; Make the org calendar start on monday
  (setq calendar-week-start-day 1)
  ;; disable org folding at launch
  (setq org-startup-folded nil)
  ;; disable truncate-lines at launch
  (setq org-startup-truncated nil)
  (setq org-startup-indented nil)
  (setq org-odd-levels-only nil)
  ;; highlight code-block
  (setq org-src-fontify-natively t)
  ;; do not add timestamp when closing todos
  (setq org-log-done nil)
  ;; start display tags after col 60
  (setq org-tags-column 0)
  ;; (setq org-fast-tag-selection-single-key t)
  (setq org-insert-heading-respect-content t)
  (setq org-hide-emphasis-markers nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-highlight-latex-and-related '(latex script entities)))

(defun yxl-org/org-mode-hook ()
  (setq line-spacing 4)
  (yxl-org-format-task-files))

(defun yxl-org/setup-capture ()
  (setq org-capture-templates
        '(("i" "life-inbox" entry
           (file+headline yxl-file-org-main "Inbox")
           "* INBOX %?\n  %i\n")
          ("t" "life-todo" entry
           (file+headline yxl-file-org-main "Inbox")
           "* TODO %?\n  %i\n")
          ("c" "config" entry
           (file+headline yxl-file-org-main "Config")
           "* INBOX %?\n  %i\n")
          ("n" "quick note" item
           (file+headline yxl-file-note-master "Quick Notes"))
          ("I" "work-inbox" entry
           (file+headline yxl-file-org-work "Inbox")
           "* INBOX %?\n  %i\n")
          ("T" "work-todo" entry
           (file+headline yxl-file-org-work "Inbox")
           "* TODO %?\n  %i\n")
          ("l" "logs" entry
           (file+datetree yxl-file-org-log)
           "* %?\n  -- %U\n  %i\n"))))

(defun yxl-org/setup-keywords ()
  (setq org-todo-keywords
        '((sequence
           "INBOX(i)"                   ;; ideas, undecided
           "QUICK(q)"                   ;; quick
           "HAVE-A-LOOK(h)"
           "TODAY(T)"                       ;; needs to be done today
           "TODO(t)"                        ;; needs to be done
           "NEXT(n)"                        ;; next in line
           "HOLD(H)"                        ;; put on hold for various reasons
           "PLAN(P)"                        ;; still under planning
           "FOLLOW-UP(f)"                   ;; follow-up results
           "SOMEDAY(s)"                     ;; not now
           "THINK(k)"                       ;; think about it
           "|" "DONE(d)" "CANCELED(C)" "FAILED(F)")
          (sequence
           "WIP(I)"
           "00(0)" "25(1)" "50(2)" "75(3)" "95(4)"
           "|" "DONE(d) ABORT(A)")))
  (setq org-todo-keyword-faces
        '(("INBOX" . (:foreground "#268bd2"))
          ("HAVE-A-LOOK" . (:foreground "#d33682"))
          ("TODAY" . (:foreground "#dc322f"))
          ("TODO" . (:foreground "#cb4b16"))
          ("HOLD" . (:foreground "#268bd2"))
          ("NEXT" . (:foreground "#6c71c4"))
          ("QUICK" . (:foreground "#6c71c4"))
          ("SOMEDAY" . (:foreground "#6c71c4"))
          ("FOLLOW-UP" . (:foreground "#6c71c4"))
          ("DOING" . (:foreground "#b58900"))
          ("WIP" . (:foreground "#b58900"))
          ("DONE" . (:foreground "#586e75"))
          ("00" . (:foreground "#deab0e"))
          ("25" . (:foreground "#b58900"))
          ("50" . (:foreground "#b58900"))
          ("75" . (:foreground "#926e00"))
          ("95" . (:foreground "#926e00"))))
  ;; (setq org-tag-faces
  ;;       '(("CAPTURE" . (:foreground "#268bd2" :slant italic))
  ;;         ("CTW" . (:foreground "#268bd2" :slant italic))
  ;;         ("WORK" . (:foreground "#268bd2" :slant italic))
  ;;         ("HOME" . (:foreground "#859900" :slant italic))
  ;;         ("CONFIG" . (:foreground "#859900" :slant italic))
  ;;         ("MAJOR" . (:foreground "#cb4b16" :slant italic))
  ;;         ("MID" . (:foreground "#b58900" :slant italic))
  ;;         ("MINOR" . (:foreground "#859900" :slant italic))))
  (setq org-tag-persistent-alist
        '(;; task group
          ("CTW" . ?T) ("WORK" . ?W) ("HOME" . ?H) ("CONFIG" . ?C)
          ;; effort required
          ("MAJOR" . ?1) ("MID" . ?2) ("MINOR" . ?3)
          ;; actions to take
          ("ISSUES" . ?i) ("HAVE_A_LOOK" . ?h) ("THINK" . ?t) ("REFACTOR" . ?r)
          ;; task propertiy
          ("URGENT" . ?u) ("KEY" . ?k) ("HARD" . ?a) ("BONUS" . ?b))))

(defun yxl-org/setup-agenda ()
  ;; agenda file
  (setq org-agenda-files yxl-org-agenda-files)
  ;; agenda view: 1 month
  (setq org-agenda-span 'month)
  ;; org agenda time grid
  (setq org-agenda-time-grid '((daily today)
                               "----------------"
                               (0900 1100 1300 1500 1700)))

  (add-to-list 'org-agenda-custom-commands
               '("0" "Life -- todo list"
                 ((todo "TODAY" ((org-agenda-files yxl-org-agenda-files-life)))
                  (todo "INBOX|QUICK|HAVE-A-LOOK" ((org-agenda-files yxl-org-agenda-files-life)))
                  (todo "TODO|NEXT" ((org-agenda-files yxl-org-agenda-files-life)))
                  (todo "DOING|00|25|50|75|95" ((org-agenda-files yxl-org-agenda-files-life)))
                  (todo "FOLLOW-UP|SOMEDAY" ((org-agenda-files yxl-org-agenda-files-life)))))
               nil)
  (add-to-list 'org-agenda-custom-commands
               '("9" "Life -- Bi-Weekly"
                 ((agenda "Agenda" ((org-agenda-ndays 14)
                                    (org-agenda-start-day "-7d")
                                    (org-agenda-files yxl-org-agenda-files-life)
                                    (org-agenda-repeating-timestamp-show-all t)))
                  (todo "TODAY" ((org-agenda-files yxl-org-agenda-files-life)))
                  (todo "INBOX|QUICK|HAVE-A-LOOK" ((org-agenda-files yxl-org-agenda-files-life)))
                  (todo "TODO|NEXT" ((org-agenda-files yxl-org-agenda-files-life)))
                  (todo "DOING|00|25|50|75|95" ((org-agenda-files yxl-org-agenda-files-life)))
                  (todo "FOLLOW-UP|SOMEDAY" ((org-agenda-files yxl-org-agenda-files-life)))))
               nil)
  (add-to-list 'org-agenda-custom-commands
               '("1" "Work -- todo list"
                 ((todo "TODAY" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "INBOX|QUICK|HAVE-A-LOOK" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "TODO|NEXT" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "DOING|00|25|50|75|95" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "FOLLOW-UP|SOMEDAY" ((org-agenda-files yxl-org-agenda-files-work)))))
               nil)
  (add-to-list 'org-agenda-custom-commands
               '("2" "Work -- 14 Days"
                 ((agenda "Agenda" ((org-agenda-ndays 14)
                                    (org-agenda-start-day "-7d")
                                    (org-agenda-files yxl-org-agenda-files-work)
                                    (org-agenda-repeating-timestamp-show-all t)))
                  (todo "TODAY" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "INBOX|QUICK|HAVE-A-LOOK" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "TODO|NEXT" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "DOING|00|25|50|75|95" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "FOLLOW-UP|SOMEDAY" ((org-agenda-files yxl-org-agenda-files-work)))))
               nil)
  (add-to-list 'org-agenda-custom-commands
               '("3" "Work -- 30 Days"
                 ((agenda "Agenda" ((org-agenda-ndays 30)
                                    (org-agenda-start-day "-7d")
                                    (org-agenda-files yxl-org-agenda-files-work)
                                    (org-agenda-repeating-timestamp-show-all t)))
                  (todo "TODAY" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "INBOX|QUICK|HAVE-A-LOOK" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "TODO|NEXT" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "DOING|00|25|50|75|95" ((org-agenda-files yxl-org-agenda-files-work)))
                  (todo "FOLLOW-UP|SOMEDAY" ((org-agenda-files yxl-org-agenda-files-work)))))
               nil))
