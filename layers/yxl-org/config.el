(defvar yxl-org-babel-languages '((emacs-lisp . t))
  "Languages to be loaded in org-babel.")



;; NOTE: Org variables are dependent on variables set in "yxl-env.el"
(setq org-todo-keywords
      '((sequence
         "INBOX(i)"   ; ideas, undecided
         "DO(T)"      ; needs to be done today
         "TODO(t)"    ; needs to be done
         "NEXT(n)"    ; next in line
         "WIP(I)"
         "HOLD(H)"    ; put on hold for various reasons
         "PLAN(P)"    ; still under planning
         "FOLLOW(f)"  ; follow-up results
         "REVIEW(r)"
         "SOMEDAY(s)" ; not now
         "|" "DONE(d)" "ABORT(A)" "FAILED(F)")))

(setq org-tag-persistent-alist
      '((:startgroup . "group")
        ("CTW") ("WORK") ("HOME")
        (:endgroup . nil)
        ("WIN") ("MAC") ("LINUX")
        (:startgroup . "effort")
        ("MAJOR") ("MID") ("MINOR")
        (:endgroup . nil)
        (:startgroup . "progress")
        ("00" . ?0) ("25" . ?2) ("50" . ?5) ("75" . ?7) ("95" . ?9)
        (:endgroup . nil)
        (:startgroup . "actions")
        ("ISSUES") ("HAVE_A_LOOK") ("THINK") ("REFACTOR")
        (:endgroup . nil)))

(setq org-agenda-custom-commands
      '(("A" "Agenda Augmented"
         ((agenda "Agenda" ((org-agenda-ndays 7)))
          (todo "DO|TODO")
          (todo "INBOX|QUICK|HAVE-A-LOOK")
          (todo "NEXT|WIP|PLAN")))
        ("0" "Agenda with general tasks"
         ((alltodo "" ((org-agenda-files (list yxl-file-org-quick))))
          (todo "DO|TODO"
                ((org-agenda-files yxl-env-org-task-files)))
          (todo "INBOX|QUICK|HAVE-A-LOOK"
                ((org-agenda-files yxl-env-org-task-files)))
          (todo "NEXT|WIP|PLAN"
                ((org-agenda-files yxl-env-org-task-files)))))))

(setq org-capture-templates
      `(;; scratch
        ("c" "scratch: checkbox" checkitem (file+headline yxl-file-org-quick "Next")
         "-  %?\n")
        ("t" "scratch: today" checkitem (file+headline yxl-file-org-quick "Today")
         "- [ ]  %?\n")
        ;; general
        ("i" "general: inbox" entry (file+headline yxl-file-org-todo "Capture")
         "* INBOX %?\n  %i\n")
        ;; jobs
        ("j" "work: jobs" entry (file+headline ,(concat yxl-path-org "projects/jobs.org") "Posts")
         "* %?\n** desc \n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n %i\n")))



(setq org-todo-keyword-faces
      `(("INBOX" . (:height 0.8 :slant italic :weight bold :foreground ,(face-foreground 'font-lock-constant-face)))
        ("DO" . (:height 0.8 :slant italic :weight bold :foreground ,(face-foreground 'font-lock-warning-face)))
        ("TODO" . (:height 0.8 :slant italic :weight bold :foreground ,(face-foreground 'font-lock-variable-name-face)))
        ("HOLD" . (:height 0.8 :slant italic :weight bold :foreground ,(face-foreground 'font-lock-string-face)))
        ("NEXT" . (:height 0.8 :slant italic :weight bold :foreground ,(face-foreground 'font-lock-constant-face)))
        ("FOLLOW" . (:height 0.8 :slant italic :weight bold :foreground ,(face-foreground 'font-lock-builtin-face)))
        ("WIP" . (:height 0.8 :slant italic :weight bold :foreground ,(face-foreground 'font-lock-builtin-face)))
        ("DONE" . (:height 0.8 :slant italic :weight bold :foreground ,(face-foreground 'font-lock-comment-face)))))

(setq org-tag-faces
      `(("CTW" . (:weight bold :foreground ,(face-foreground 'font-lock-function-name-face)))
        ("WORK" . (:weight bold :foreground ,(face-foreground 'font-lock-function-name-face)))
        ("HOME" . (:weight bold :foreground ,(face-foreground 'font-lock-constant-face)))
        ("HAVE_A_LOOK" . (:weight bold :foreground ,(face-foreground 'font-lock-variable-name-face)))
        ("MAJOR" . (:weight bold :foreground ,(face-foreground 'font-lock-warning-face)))
        ("MID" . (:weight bold :foreground ,(face-foreground 'font-lock-variable-name-face)))
        ("MINOR" . (:weight bold :foreground ,(face-foreground 'font-lock-string-face)))
        ("00" . (:weight bold :foreground "#deab0e"))
        ("25" . (:weight bold :foreground "#b58900"))
        ("50" . (:weight bold :foreground "#b58900"))
        ("75" . (:weight bold :foreground "#926e00"))
        ("95" . (:weight bold :foreground "#926e00"))))
