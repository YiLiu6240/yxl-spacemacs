(defun magit-toggle-commit-show-diff ()
  (interactive)
  (setq magit-commit-show-diff (not magit-commit-show-diff))
  (message (format "magit-commit-show-diff: %s" magit-commit-show-diff)))

(defun magit-toggle-fullscreen ()
  (interactive)
  (if (eq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
      (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
  (message "magit-display-buffer-function %s" magit-display-buffer-function))

(defun magit-status-fullscreen ()
  (interactive)
  (let ((magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
    (magit-status)))

(defun magit-insert-standup-commits (&optional collapse)
  "Insert section showing recent commits. From yesterday to today."
  (let* ((range "--since=yesterday.midnight"))
    (magit-insert-section (recent range collapse)
      (magit-insert-heading "Standup")
      (magit-insert-log range
                        (cons (format "-n%d" magit-log-section-commit-count)
                              (--remove (string-prefix-p "-n" it)
                                        magit-log-section-arguments))))))

(defun git-gutter-reshape (gutter)
      "Re-shape gutter for `ivy-read'.
Source: http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html"
      (let* ((linenum-start (aref gutter 3))
             (linenum-end (aref gutter 4))
             (target-line "")
             (target-linenum 1)
             (tmp-line "")
             (max-line-length 0))
        (save-excursion
          (while (<= linenum-start linenum-end)
            (goto-line linenum-start)
            (setq tmp-line (replace-regexp-in-string
                            "^[ \t]*" ""
                            (buffer-substring (line-beginning-position)
                                              (line-end-position))))
            (when (> (length tmp-line) max-line-length)
              (setq target-linenum linenum-start)
              (setq target-line tmp-line)
              (setq max-line-length (length tmp-line)))

            (setq linenum-start (1+ linenum-start))))
        ;; build (key . linenum-start)
        (cons (format "%s %d: %s"
                      (if (eq 'deleted (aref gutter 1)) "-" "+")
                      target-linenum target-line)
              target-linenum)))

(defun git-gutter-ivy-select ()
  "Source: http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html"
  (interactive)
  (if git-gutter:diffinfos
      (ivy-read "git-gutters:"
                (mapcar 'git-gutter-reshape git-gutter:diffinfos)
                :action (lambda (e)
                          ;; ivy9+ keep `(car e)'
                          ;; ivy8- strip the `(car e)'
                          ;; we handle both data structure
                          (unless (numberp e) (setq e (cdr e)))
                          (goto-line e)))
    (message "NO git-gutters!")))

(defun git-timemachine-show-selected-revision ()
  "Show last (current) revision of file.
Source: http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html"
  (interactive)
  (let* ((collection (mapcar (lambda (rev)
                               ;; re-shape list for the ivy-read
                               (cons (concat (substring-no-properties (nth 0 rev) 0 7)
                                             "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                             (git-timemachine--revisions))))
    (ivy-read "commits:"
              collection
              :action (lambda (rev)
                        ;; compatible with ivy 9+ and ivy 8
                        (unless (string-match-p "^[a-z0-9]*$" (car rev))
                          (setq rev (cdr rev)))
                        (git-timemachine-show-revision rev)))))

(defun git-timemachine-ivy-select ()
  "Open git snapshot with the selected version.  Based on ivy-mode.
Source: http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html"
  (interactive)
  (git-timemachine--start #'git-timemachine-show-selected-revision))
