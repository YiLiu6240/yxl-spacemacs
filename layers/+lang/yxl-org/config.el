(defvar yxl-org-babel-languages '((emacs-lisp . t))
  "Languages to be loaded in org-babel.")



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
         "PROJ(p)"       ; Its a project, not an action
         "???(?)"
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



(setq org-todo-keyword-faces
      `(("INBOX" . (:slant italic :weight bold :foreground ,(face-foreground 'font-lock-constant-face)))
        ("DO" . (:slant italic :weight bold :foreground ,(face-foreground 'font-lock-warning-face)))
        ("TODO" . (:slant italic :weight bold :foreground ,(face-foreground 'font-lock-variable-name-face)))
        ("HOLD" . (:slant italic :weight bold :foreground ,(face-foreground 'font-lock-string-face)))
        ("NEXT" . (:slant italic :weight bold :foreground ,(face-foreground 'font-lock-constant-face)))
        ("FOLLOW" . (:slant italic :weight bold :foreground ,(face-foreground 'font-lock-builtin-face)))
        ("WIP" . (:slant italic :weight bold :foreground ,(face-foreground 'font-lock-builtin-face)))
        ("DONE" . (:slant italic :weight bold :foreground ,(face-foreground 'font-lock-comment-face)))))

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
