(defun my-org-agenda-work ()
  (interactive)
  (org-agenda nil "1"))

(defun my-org-agenda-life ()
  (interactive)
  (org-agenda nil "0"))

(defun my-org-log ()
  (interactive)
  (find-file yxl-file-org-log))

(defun yxl-org/refile-to-scratch ()
  (interactive)
  (let ((org-refile-targets '((yxl-file-org-scratch :maxlevel . 1)
                              (nil :maxlevel . 1))))
    (org-refile)))

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
  (setq org-fontify-whole-heading-line t)
  (setq org-refile-use-outline-path t)
  (setq org-refile-targets '((nil :maxlevel . 1)
                             (yxl-env-org-task-files :maxlevel . 1)))
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . (lambda (path str)
                                         (yxl-open-file-external path)))
                        (t . (lambda (path str)
                               (yxl-open-file-external path)))))
  (setq org-reveal-root
        (format "file:///%s"
                (expand-file-name "~/dotfiles/external/reveal.js/")))
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "b" nil
    "m" nil)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "bb" #'org-edit-src-code
    "bn" #'org-next-block
    "bj" #'org-next-block
    "bp" #'org-previous-block
    "bk" #'org-previous-block
    "bs" #'org-babel-switch-to-session
    "mm" #'org-toggle-latex-fragment
    "r" #'yxl-org-refile-visible
    "R" #'yxl-org/refile-to-scratch)
  (spacemacs/declare-prefix-for-mode 'org-mode "b" "src-block")
  (spacemacs/declare-prefix-for-mode 'org-mode "m" "math"))

(defun yxl-org/org-mode-hook ()
  (setq evil-auto-indent nil))
  ;; (setq line-spacing 4)
  ;; NOTE: buggy, disable for now
  ;; (yxl-org-format-task-files)


(defun yxl-org/setup-bindings ()
  (evil-define-key 'normal org-mode-map
    "t" 'org-todo
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
    "T" (lambda ()
          (interactive)
          (evil-org-eol-call
           (lambda() (org-insert-todo-heading nil))))
    "o" 'evil-open-below
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
        `(;; generalised
          ("i" "general: inbox" entry (file+headline yxl-file-org-todo "Capture")
           "* INBOX %?\n  %i\n")
          ("c" "general: inbox" entry (file+headline yxl-file-org-todo "Capture")
           "* INBOX %?\n  %i\n")
          ("t" "general: todo" entry (file+headline yxl-file-org-todo "Capture")
           "* TODO %?\n  %i\n")
          ("j" "work: jobs" entry (file+headline ,(concat yxl-path-org "projects/jobs.org") "Posts")
           "* %?\n** desc \n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n %i\n")
          ;; note and log
          ("n" "quick note" item (file+headline yxl-file-note-master "Quick Notes"))
          ("l" "logs" entry (file+datetree yxl-file-org-log)
           "* %?\n  -- %U\n  %i\n"))))

(defun yxl-org/setup-keywords ()
  (setq org-todo-keywords
        '((sequence
           "INBOX(i)"                   ;; ideas, undecided
           "DO(T)"                       ;; needs to be done today
           "TODO(t)"                        ;; needs to be done
           "NEXT(n)"                        ;; next in line
           "HOLD(H)"                        ;; put on hold for various reasons
           "WIP(I)"
           "PROJ(p)"
           "PLAN(P)"                        ;; still under planning
           "FOLLOW(f)"                   ;; follow-up results
           "SOMEDAY(s)"                     ;; not now
           "|" "DONE(d)" "CANCELED(C)" "ABORT(A)" "FAILED(F)")))
  (setq org-todo-keyword-faces
        `(("INBOX" . (:height 0.8 :slant italic :weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
          ("DO" . (:height 0.8 :slant italic :weight bold :foreground ,(face-attribute 'font-lock-warning-face :foreground)))
          ("TODO" . (:height 0.8 :slant italic :weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
          ("HOLD" . (:height 0.8 :slant italic :weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
          ("NEXT" . (:height 0.8 :slant italic :weight bold :foreground ,(face-attribute 'font-lock-constant-face :foreground)))
          ("FOLLOW" . (:height 0.8 :slant italic :weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
          ("PROJ" . (:height 0.8 :slant italic :weight bold :foreground ,(face-attribute 'font-lock-type-face :foreground)))
          ("WIP" . (:height 0.8 :slant italic :weight bold :foreground ,(face-attribute 'font-lock-type-face :foreground)))
          ("DONE" . (:height 0.8 :slant italic :weight bold :foreground ,(face-attribute 'font-lock-comment-face :foreground)))))
  (setq org-tag-faces
        `(("CTW" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
          ("WORK" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
          ("HOME" . (:weight bold :foreground ,(face-attribute 'font-lock-constant-face :foreground)))
          ("HAVE_A_LOOK" . (:weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
          ("MAJOR" . (:weight bold :foreground ,(face-attribute 'font-lock-warning-face :foreground)))
          ("MID" . (:weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
          ("MINOR" . (:weight bold :foreground ,(face-attribute 'font-lock-string-face :foreground)))
          ("00" . (:weight bold :foreground "#deab0e"))
          ("25" . (:weight bold :foreground "#b58900"))
          ("50" . (:weight bold :foreground "#b58900"))
          ("75" . (:weight bold :foreground "#926e00"))
          ("95" . (:weight bold :foreground "#926e00"))))
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

(defun yxl-org/setup-extra-fontlock ()
  ;; source: https://github.com/hlissner/.emacs.d/blob/master/modules/lang/org/config.el
  (setq org-font-lock-extra-keywords
        (delete '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                  (0 (org-get-checkbox-statistics-face) t))
                org-font-lock-extra-keywords))
  (nconc org-font-lock-extra-keywords
         '( ;; Make checkbox statistic cookies respect underlying faces
           ("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
            (0 (org-get-checkbox-statistics-face) prepend))
           ;; I like how org-mode fontifies checked TODOs and want this to extend to
           ;; checked checkbox items:
           ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
            1 'org-headline-done prepend)
           ;; make plain list bullets stand out
           ("^ *\\([-+]\\|[0-9]+[).]\\) " 1 'org-list-dt append)
           ;; and separators/dividers
           ("^ *\\(-----+\\)$" 1 'org-meta-line)
           ;; custom #hashtags & @at-tags for another level of organization
           ;; TODO refactor this into a single rule
           ("\\s-\\(#[^ \n]+\\)" 1 'org-tag)
           ("\\s-\\(@[^ \n]+\\)" 1 'org-special-keyword))))

(defun yxl-org/setup-agenda ()
  ;; agenda file
  (setq org-agenda-files (append yxl-env-org-files
                                 (directory-files "~/local-repo" t)))
  ;; agenda view: 1 month
  (setq org-agenda-span 'week)
  (setq org-agenda-format-date 'yxl-org-agenda-format-date-aligned)
  ;; org agenda time grid
  (setq org-agenda-time-grid '((daily today)
                               "----------------"
                               (0900 1100 1300 1500 1700)))
  (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
    "E" #'org-agenda-entry-text-mode
    "." #'spacemacs/org-agenda-transient-state/body)
  (dolist (agenda yxl-org-agenda-commands)
    (add-to-list 'org-agenda-custom-commands agenda t))
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t%s")
          (timeline . "  %s")
          ;; (todo . " %i %-12:c %b")
          (todo . " %(format \"%s/%s\" (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))) (file-name-nondirectory (buffer-file-name))) ")
          (tags . " %i %-12:c")
          (search . " %i %-12:c"))))

(defun yxl-org/setup-latex ()
  (setq org-preview-latex-image-directory ".ltximg/"))

(defun yxl-org/setup-babel ()
  (setq-default org-export-babel-evaluate nil)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-preserve-indentation t)
  (setq org-src-fontify-natively nil)
  (setq org-src-tab-acts-natively t)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  (org-babel-do-load-languages 'org-babel-load-languages '((dot . t)
                                                           (latex .t)
                                                           (octave .t)
                                                           (org .t)
                                                           (perl .t)
                                                           (python .t)
                                                           (ruby .t)
                                                           (sh .t)
                                                           (R .t)
                                                           (ipython . t)
                                                           (scala . t))))
