;; TODO: update namings of these
(defun my-org-agenda-work ()
  (interactive)
  (org-agenda nil "1"))

(defun my-org-agenda-life ()
  (interactive)
  (org-agenda nil "0"))

(defun my-org-log ()
  (interactive)
  (find-file yxl-file-org-log)
  (visual-fill-column-mode t)
  (setq visual-fill-column-center-text t))

(defun yxl-org/toggle-org-src-fontify-natively ()
  "Toggle `org-src-fontify-natively'. Remember to reload the related buffer(s)."
  (interactive)
  (setq org-src-fontify-natively (not org-src-fontify-natively))
  (message (format "org-src-fontify-natively: %s" org-src-fontify-natively)))

(defun yxl-org/refile-to-scratch ()
  (interactive)
  (let ((org-refile-targets '((yxl-file-org-checkbox :maxlevel . 1)
                              (nil :maxlevel . 1))))
    (org-refile)))

(defun yxl-org/insert-source-block ()
  "Insert an org source block.

When there is a property \"#+property: src-header <property>\",
where <property> is anything you want to add to the source block,
add this property to the new source block by default (this behavior is
overridden by a prefix arg)."
  (interactive)
  (let* ((src-header-prop (unless current-prefix-arg
                            (cdr (assoc "src-header" org-file-properties))))
         (src-header-str (or src-header-prop "")))
    (insert (format "#+begin_src %s\n\n#+end_src" src-header-str))
    (if src-header-prop
        (progn
          (forward-line -1))
      (progn
        (forward-line -2)
        (end-of-line)))))

(defun yxl-org/insert-source-block-and-edit ()
  "Insert an org source block then invoke `'."
  (interactive)
  (yxl-org/insert-source-block)
  (org-edit-src-code))

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
  ;; do not add timestamp when closing todos
  (setq org-log-done nil)
  ;; start display tags after col 60
  (setq org-tags-column 0)
  ;; (setq org-fast-tag-selection-single-key t)
  (setq org-insert-heading-respect-content t)
  (setq org-hide-emphasis-markers nil)
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-blank-before-new-entry '((heading . auto)
                                     (plain-list-item . auto)))
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-refile-use-outline-path t)
  (setq org-refile-targets '((nil :maxlevel . 1)
                             (yxl-env-org-task-files :maxlevel . 1)))
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-cycle-separator-lines 1)
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . (lambda (path str)
                                         (yxl-open-file-external path)))
                        (t . (lambda (path str)
                               (yxl-open-file-external path))))))

(defun yxl-org/org-mode-hook ()
  (setq evil-auto-indent nil))
  ;; (setq line-spacing 4)
  ;; NOTE: buggy, disable for now
  ;; (yxl-org-format-task-files)

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
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-sticky t)
  (setq org-agenda-start-on-weekday nil)
  ;; org agenda time grid
  (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
    "E" #'org-agenda-entry-text-mode
    "." #'spacemacs/org-agenda-transient-state/body)
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t%s")
          (timeline . "  %s")
          ;; (todo . " %i %-12:c %b")
          (todo . " %(format \"%s/%s\" (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))) (file-name-nondirectory (buffer-file-name))) ")
          (tags . " %i %-12:c")
          (search . " %i %-12:c"))))

(defun yxl-org/setup-latex ()
  (setq org-preview-latex-image-directory ".ltximg/")
  ;; Making latex formulae BIGGER
  ;; https://stackoverflow.com/questions/11272236/how-to-make-formule-bigger-in-org-mode-of-emacs
  (setq org-format-latex-options
        (plist-put (default-value 'org-format-latex-options)
                   :scale 2.0)))

(defun yxl-org/setup-babel ()
  (setq-default org-export-babel-evaluate nil)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-preserve-indentation t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'other-window)
  (add-to-list 'display-buffer-alist '("\\*Org Src" display-buffer-at-bottom))
  (define-key org-src-mode-map
    (kbd "C-c f") #'yxl-prog/evil-wrap-line-f-print)
  (define-key org-src-mode-map
    (kbd "C-c F") #'yxl-prog/evil-wrap-line-f)
  ;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  ;; Force going to normal state after finishing source block edit
  (with-eval-after-load 'org-src
    (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
      "." #'yxl-org/edit-src-exit-and-execute)
    (advice-add 'org-edit-src-exit :after (lambda nil (evil-normal-state)))
    (advice-add 'org-edit-src-abort :after (lambda nil (evil-normal-state)))))

(defun yxl-org/babel-result-show-all ()
  "The interactive version of `org-babel-show-result-all'"
  (interactive)
  (org-babel-show-result-all))

(defun yxl-org/setup-minor-modes ()
  (define-minor-mode yxl-org/ob-ipython-helper-mode
    "Helper configs in org-mode with ob-ipython"
    :keymap
    (let ((map (make-sparse-keymap)))
      (define-key map
        (kbd "C-c h") #'ob-ipython-inspect)
      (define-key map
        (kbd "C-c k") #'ob-ipython-signature-function)
      (define-key map (kbd "C-c f")
        #'yxl-prog/evil-wrap-line-f-print)
      (define-key map (kbd "C-c F")
        #'yxl-prog/evil-wrap-line-f)
      map)
    (add-to-list 'company-backends-org-mode 'company-ob-ipython))
  (define-minor-mode yxl-org/ob-R-helper-mode
    "Helper configs in org-mode with ob-R"
    :keymap
    (let ((map (make-sparse-keymap)))
      (define-key map
        (kbd "M--") (lambda nil (interactive)
                      (if (equal (string (preceding-char)) " ")
                          (insert "<- ")
                        (insert " <- "))))
      (define-key map
        (kbd "M-=") (lambda nil (interactive)
                      (if (equal (string (preceding-char)) " ")
                          (insert "=> ")
                        (insert " => "))))
      (define-key map
        (kbd "S-RET") (lambda nil (interactive)
                        (if (equal (string (preceding-char)) " ")
                            (insert "%>% ")
                          (insert " %>% "))))
      (define-key map (kbd "C-c f")
        #'yxl-prog/evil-wrap-line-f-print)
      (define-key map (kbd "C-c F")
        #'yxl-prog/evil-wrap-line-f)
      (define-key map
        (kbd "C-c h") #'ess-help)
      map)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "hh" #'ess-help))
  (define-minor-mode yxl-org/ob-scala-helper-mode
    "Helper configs in org-mode with ob-scala"
    :keymap
    (let ((map (make-sparse-keymap)))
      (define-key map
        (kbd "M--") (lambda nil (interactive)
                      (if (equal (string (preceding-char)) " ")
                          (insert "<- ")
                        (insert " <- "))))
      (define-key map
        (kbd "M-_") (lambda nil (interactive)
                      (if (equal (string (preceding-char)) " ")
                          (insert "-> ")
                        (insert " -> "))))
      (define-key map
        (kbd "M-=") (lambda nil (interactive)
                      (if (equal (string (preceding-char)) " ")
                          (insert "=> ")
                        (insert " => "))))
      (define-key map (kbd "C-c f")
        #'yxl-prog/evil-wrap-line-f-print)
      (define-key map (kbd "C-c F")
        #'yxl-prog/evil-wrap-line-f)
      map))
  (define-minor-mode yxl-org/ob-clojure-helper-mode
    "Helper configs in org-mode with ob-clojure"
    :keymap
    (let ((map (make-sparse-keymap)))
      (define-key map
        (kbd "C-c f") #'yxl-prog/evil-wrap-line-f-lisp-print)
      (define-key map
        (kbd "C-c F") #'yxl-prog/evil-wrap-line-f-lisp)
      map)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "hh" #'cider-doc)))

(defun yxl-org/load-ob-helper ()
  "Detect and load a ob helper.
The helper is set in the format of

#+property: ob-helper 'yxl-org/ob-R-helper-mode

in the file head.
The rationale of loading a helper in this way instead of using
file local variables is purely for aesthetic reasons."
  (let ((ob-helper (cdr (assoc "ob-helper" org-file-properties))))
    (when ob-helper
      (if (fboundp (intern ob-helper))
          (progn
            (message (format "ob-helper: %s" ob-helper))
            (funcall (intern ob-helper)))
        (warn (format "helper mode %s not found." ob-helper))))))

(defun yxl-org/edit-src-exit-and-execute ()
  "Finish source block and execute it."
  (interactive)
  (org-edit-src-exit)
  (org-ctrl-c-ctrl-c))
