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
  (setq org-highlight-latex-and-related '(latex script entities))
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
          (:startgroup . "context")
          ("WIN") ("MAC") ("LINUX")
          (:endgroup . nil)
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
  (setq org-agenda-files yxl-org-files)
  ;; agenda view: 1 month
  (setq org-agenda-span 'month)
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

(defun yxl-org/patch ()
  ;; add padding to tag selection window
  (defun org-fast-tag-selection (current inherited table &optional todo-table)
  "Fast tag selection with single keys.
CURRENT is the current list of tags in the headline, INHERITED is the
list of inherited tags, and TABLE is an alist of tags and corresponding keys,
possibly with grouping information.  TODO-TABLE is a similar table with
TODO keywords, should these have keys assigned to them.
If the keys are nil, a-z are automatically assigned.
Returns the new tags string, or nil to not change the current settings."
  (let* ((fulltable (append table todo-table))
     (maxlen (apply 'max (mapcar
                  (lambda (x)
                (if (stringp (car x)) (string-width (car x)) 0))
                  fulltable)))
     (buf (current-buffer))
     (expert (eq org-fast-tag-selection-single-key 'expert))
     (buffer-tags nil)
     (fwidth (+ maxlen 3 1 3))
     (ncol (/ (- (window-width) 4) fwidth))
     (i-face 'org-done)
     (c-face 'org-todo)
     tg cnt e c char c1 c2 ntable tbl rtn
     ov-start ov-end ov-prefix
     (exit-after-next org-fast-tag-selection-single-key)
     (done-keywords org-done-keywords)
     groups ingroup intaggroup)
    (save-excursion
      (beginning-of-line 1)
      (if (looking-at ".*[ \t]\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$")
      (setq ov-start (match-beginning 1)
        ov-end (match-end 1)
        ov-prefix "")
    (setq ov-start (1- (point-at-eol))
          ov-end (1+ ov-start))
    (skip-chars-forward "^\n\r")
    (setq ov-prefix
          (concat
           (buffer-substring (1- (point)) (point))
           (if (> (current-column) org-tags-column)
           " "
         (make-string (- org-tags-column (current-column)) ?\ ))))))
    (move-overlay org-tags-overlay ov-start ov-end)
    (save-window-excursion
      (if expert
      (set-buffer (get-buffer-create " *Org tags*"))
    (delete-other-windows)
    (set-window-buffer (split-window-vertically) (get-buffer-create " *Org tags*"))
    (org-switch-to-buffer-other-window " *Org tags*"))
      (erase-buffer)
      (setq-local org-done-keywords done-keywords)
      (org-fast-tag-insert "Inherited" inherited i-face "\n")
      (org-fast-tag-insert "Current" current c-face "\n\n")
      (org-fast-tag-show-exit exit-after-next)
      (org-set-current-tags-overlay current ov-prefix)
      (setq tbl fulltable char ?a cnt 0)
      (while (setq e (pop tbl))
    (cond
     ((eq (car e) :startgroup)
      (push '() groups) (setq ingroup t)
      (unless (zerop cnt)
        (setq cnt 0)
        (insert "\n"))
      (insert (if (cdr e) (format "%s: " (cdr e)) "") "{ "))
     ((eq (car e) :endgroup)
      (setq ingroup nil cnt 0)
      (insert "}" (if (cdr e) (format " (%s) " (cdr e)) "") "\n"))
     ((eq (car e) :startgrouptag)
      (setq intaggroup t)
      (unless (zerop cnt)
        (setq cnt 0)
        (insert "\n"))
      (insert "[ "))
     ((eq (car e) :endgrouptag)
      (setq intaggroup nil cnt 0)
      (insert "]\n"))
     ((equal e '(:newline))
      (unless (zerop cnt)
        (setq cnt 0)
        (insert "\n")
        (setq e (car tbl))
        (while (equal (car tbl) '(:newline))
          (insert "\n")
          (setq tbl (cdr tbl)))))
     ((equal e '(:grouptags)) (insert " : "))
     (t
      (setq tg (copy-sequence (car e)) c2 nil)
      (if (cdr e)
          (setq c (cdr e))
        ;; automatically assign a character.
        (setq c1 (string-to-char
              (downcase (substring
                 tg (if (= (string-to-char tg) ?@) 1 0)))))
        (if (or (rassoc c1 ntable) (rassoc c1 table))
        (while (or (rassoc char ntable) (rassoc char table))
          (setq char (1+ char)))
          (setq c2 c1))
        (setq c (or c2 char)))
      (when ingroup (push tg (car groups)))
      (setq tg (org-add-props tg nil 'face
                  (cond
                   ((not (assoc tg table))
                    (org-get-todo-face tg))
                   ((member tg current) c-face)
                   ((member tg inherited) i-face))))
      (when (equal (caar tbl) :grouptags)
        (org-add-props tg nil 'face 'org-tag-group))
      (when (and (zerop cnt) (not ingroup) (not intaggroup)) (insert " "))
      (insert "[" c "] " tg (make-string
                 (- fwidth 4 (length tg)) ?\ ))
      (push (cons tg c) ntable)
      (when (= (cl-incf cnt) ncol)
        (insert "\n")
        (when (or ingroup intaggroup) (insert " "))
        (setq cnt 0)))))
      (setq ntable (nreverse ntable))
      (insert "\n")
      (insert "-----------\n")
      (goto-char (point-min))
      (unless expert (org-fit-window-to-buffer))
      (setq rtn
        (catch 'exit
          (while t
        (message "[a-z..]:Toggle [SPC]:clear [RET]:accept [TAB]:free [!] %sgroups%s"
             (if (not groups) "no " "")
             (if expert " [C-c]:window" (if exit-after-next " [C-c]:single" " [C-c]:multi")))
        (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
        (setq org-last-tag-selection-key c)
        (cond
         ((= c ?\r) (throw 'exit t))
         ((= c ?!)
          (setq groups (not groups))
          (goto-char (point-min))
          (while (re-search-forward "[{}]" nil t) (replace-match " ")))
         ((= c ?\C-c)
          (if (not expert)
              (org-fast-tag-show-exit
               (setq exit-after-next (not exit-after-next)))
            (setq expert nil)
            (delete-other-windows)
            (set-window-buffer (split-window-vertically) " *Org tags*")
            (org-switch-to-buffer-other-window " *Org tags*")
            (org-fit-window-to-buffer)))
         ((or (= c ?\C-g)
              (and (= c ?q) (not (rassoc c ntable))))
          (delete-overlay org-tags-overlay)
          (setq quit-flag t))
         ((= c ?\ )
          (setq current nil)
          (when exit-after-next (setq exit-after-next 'now)))
         ((= c ?\t)
          (condition-case nil
              (setq tg (completing-read
                "Tag: "
                (or buffer-tags
                    (with-current-buffer buf
                      (setq buffer-tags
                        (org-get-buffer-tags))))))
            (quit (setq tg "")))
          (when (string-match "\\S-" tg)
            (cl-pushnew (list tg) buffer-tags :test #'equal)
            (if (member tg current)
            (setq current (delete tg current))
              (push tg current)))
          (when exit-after-next (setq exit-after-next 'now)))
         ((setq e (rassoc c todo-table) tg (car e))
          (with-current-buffer buf
            (save-excursion (org-todo tg)))
          (when exit-after-next (setq exit-after-next 'now)))
         ((setq e (rassoc c ntable) tg (car e))
          (if (member tg current)
              (setq current (delete tg current))
            (cl-loop for g in groups do
                 (when (member tg g)
                   (dolist (x g) (setq current (delete x current)))))
            (push tg current))
          (when exit-after-next (setq exit-after-next 'now))))

        ;; Create a sorted list
        (setq current
              (sort current
                (lambda (a b)
                  (assoc b (cdr (memq (assoc a ntable) ntable))))))
        (when (eq exit-after-next 'now) (throw 'exit t))
        (goto-char (point-min))
        (beginning-of-line 2)
        (delete-region (point) (point-at-eol))
        (org-fast-tag-insert "Current" current c-face)
        (org-set-current-tags-overlay current ov-prefix)
        (while (re-search-forward "\\[.\\] \\([[:alnum:]_@#%]+\\)" nil t)
          (setq tg (match-string 1))
          (add-text-properties
           (match-beginning 1) (match-end 1)
           (list 'face
             (cond
              ((member tg current) c-face)
              ((member tg inherited) i-face)
              (t (get-text-property (match-beginning 1) 'face))))))
        (goto-char (point-min)))))
      (delete-overlay org-tags-overlay)
      (if rtn
      (mapconcat 'identity current ":")
    nil)))))
(defun yxl-org/setup-babel ()
  (setq-default org-export-babel-evaluate nil))
