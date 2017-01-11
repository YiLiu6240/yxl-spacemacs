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
  (setq org-agenda-sticky t)
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-blank-before-new-entry nil)
  (setq org-refile-use-outline-path t)
  (setq org-refile-targets '((nil :maxlevel . 1)
                             (yxl-file-org-scratch :maxlevel . 1)))
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "or" #'yxl-org-refile-visible))

(defun yxl-org/org-mode-hook ()
  ;; (setq line-spacing 4)
  ;; NOTE: buggy, disable for now
  ;; (yxl-org-format-task-files)
  )

(defun yxl-org/setup-bindings ()
  (evil-define-key 'normal org-mode-map
    "t" 'org-todo
    "-" 'dired-jump
    "_" 'projectile-dired
    "gh" 'outline-up-heading
    "gp" 'outline-previous-heading
    "gj" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
             'org-forward-same-level
           'org-forward-heading-same-level)
    "gk" (if (fboundp 'org-backward-same-level)
             'org-backward-same-level
           'org-backward-heading-same-level)
    "gl" 'outline-next-visible-heading
    "T" '(lambda () (interactive) (evil-org-eol-call (lambda() (org-insert-todo-heading nil))))
    "o" '(lambda () (interactive) (evil-org-eol-call 'clever-insert-item))
    "O" 'evil-open-above
    "$" 'org-end-of-line
    "^" 'org-beginning-of-line
    "<" 'org-metaleft
    ">" 'org-metaright
    (kbd "TAB") 'org-cycle)
  (mapc (lambda (state)
          (evil-define-key state org-mode-map
            (kbd "M-l") 'org-metaright
            (kbd "M-h") 'org-metaleft
            (kbd "M-k") 'org-metaup
            (kbd "M-j") 'org-metadown
            (kbd "M-L") 'org-shiftmetaright
            (kbd "M-H") 'org-shiftmetaleft
            (kbd "M-K") 'org-shiftmetaup
            (kbd "M-J") 'org-shiftmetadown
            (kbd "M-o") '(lambda () (interactive)
                           (evil-org-eol-call
                            '(lambda()
                               (org-insert-heading)
                               (org-metaright))))
            (kbd "M-t") '(lambda () (interactive)
                           (evil-org-eol-call
                            '(lambda()
                               (org-insert-todo-heading nil)
                               (org-metaright))))))
        '(normal insert)))

(defun yxl-org/setup-capture ()
  (setq org-capture-templates
        '(("i" "life-inbox" entry
           (file+headline yxl-file-org-main "Inbox")
           "* INBOX %?\n  %i\n")
          ("t" "life-todo" entry
           (file+headline yxl-file-org-main "Inbox")
           "* TODO %?\n  %i\n")
          ("c" "config" entry
           (file+headline yxl-file-org-config "Config")
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
           "TODAY(T)"                       ;; needs to be done today
           "TODO(t)"                        ;; needs to be done
           "NEXT(n)"                        ;; next in line
           "HOLD(H)"                        ;; put on hold for various reasons
           "WIP(I)"
           "PROJ(p)"
           "PLAN(P)"                        ;; still under planning
           "FOLLOW-UP(f)"                   ;; follow-up results
           "SOMEDAY(s)"                     ;; not now
           "|" "DONE(d)" "CANCELED(C)" "ABORT(A)" "FAILED(F)")))
  (setq org-todo-keyword-faces
        `(("INBOX" . (:foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
          ("TODAY" . (:foreground ,(face-attribute 'font-lock-warning-face :foreground)))
          ("TODO" . (:foreground ,(face-attribute 'font-lock-warning-face :foreground)))
          ("HOLD" . (:foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
          ("NEXT" . (:foreground ,(face-attribute 'font-lock-constant-face :foreground)))
          ("QUICK" . (:foreground ,(face-attribute 'font-lock-constant-face :foreground)))
          ("FOLLOW-UP" . (:foreground ,(face-attribute 'font-lock-constant-face :foreground)))
          ("PROJ" . (:foreground ,(face-attribute 'font-lock-type-face :foreground)))
          ("WIP" . (:foreground ,(face-attribute 'font-lock-type-face :foreground)))
          ("DONE" . (:foreground ,(face-attribute 'font-lock-comment-face :foreground)))))
  (setq org-tag-faces
        `(("CTW" . (:foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
          ("WORK" . (:foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
          ("HOME" . (:foreground ,(face-attribute 'font-lock-constant-face :foreground)))
          ("HAVE_A_LOOK" . (:foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
          ("MAJOR" . (:foreground ,(face-attribute 'font-lock-warning-face :foreground)))
          ("MID" . (:foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
          ("MINOR" . (:foreground ,(face-attribute 'font-lock-string-face :foreground)))
          ("00" . (:foreground "#deab0e"))
          ("25" . (:foreground "#b58900"))
          ("50" . (:foreground "#b58900"))
          ("75" . (:foreground "#926e00"))
          ("95" . (:foreground "#926e00"))))
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
          (:endgroup . nil))))

(defun yxl-org/setup-agenda ()
  ;; agenda file
  (setq org-agenda-files yxl-env-org-files)
  ;; agenda view: 1 month
  (setq org-agenda-span 'month)
  (setq org-agenda-format-date 'yxl-org-agenda-format-date-aligned)
  ;; org agenda time grid
  (setq org-agenda-time-grid '((daily today)
                               "----------------"
                               (0900 1100 1300 1500 1700)))
  (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
    "E" #'org-agenda-entry-text-mode)
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

(defun yxl-org/setup-babel ()
  (setq-default org-export-babel-evaluate nil))
