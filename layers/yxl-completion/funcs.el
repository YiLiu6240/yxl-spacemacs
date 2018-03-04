;; view buffer in horizontal/vertical split
;; https://github.com/emacs-helm/helm/issues/1100
(defun helm-buffer-switch-to-new-window-v (_candidate)
  "Display buffers in new windows."
  (dolist (buf (helm-marked-candidates))
    (select-window (split-window-right))
    (switch-to-buffer buf)))

(defun helm-buffer-switch-to-new-window-h (_candidate)
  "Display buffers in new windows."
  (dolist (buf (helm-marked-candidates))
    (select-window (split-window-below))
    (switch-to-buffer buf)))

(defun helm-buffer-switch-new-window-v ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-buffer-switch-to-new-window-v)))

(defun helm-buffer-switch-new-window-h ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-buffer-switch-to-new-window-h)))

(defun helm-display-window-vertical (buffer)
  (let ((display-buffer-alist
         (list '("*.*helm.**"
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (side . left)
                 (window-width . 0.3)
                 (window-height . 0.4))))
        (helm-split-window-default-side 'left))
    (helm-default-display-buffer buffer)))

(defun helm-find-files-vertical ()
  (interactive)
  (let ((helm-display-function #'helm-display-window-vertical))
    (call-interactively #'helm-find-files)))

(defun yxl-completion/counsel-patch ()
  "This is a hack to make current spacemacs compatible with upstream changes
in counsel."
  (defun spacemacs//counsel-async-filter (process str)
    (with-current-buffer (process-buffer process)
      (insert str))
    (when (or (null spacemacs--counsel-initial-cands-shown)
              (time-less-p
               ;; 0.5s
               '(0 0 500000 0)
               (time-since counsel--async-time)))
      (let (size display-now)
        (with-current-buffer (process-buffer process)
          (goto-char (point-min))
          (setq size (- (buffer-size) (forward-line (buffer-size))))
          (when (and (null spacemacs--counsel-initial-cands-shown)
                     (> size spacemacs--counsel-initial-number-cand))
            (setq ivy--all-candidates
                  (split-string (buffer-string) "\n" t))
            (setq display-now t)
            (setq spacemacs--counsel-initial-cands-shown t)))
        (let ((ivy--prompt
               (format (ivy-state-prompt ivy-last)
                       size)))
          (if display-now
              (ivy--insert-minibuffer
               (ivy--format ivy--all-candidates))
            (ivy--insert-prompt))))
      (setq counsel--async-time (current-time))))

  (defun spacemacs//make-counsel-search-function (tool)
    (lexical-let ((base-cmd
                   (cdr (assoc-string tool spacemacs--counsel-commands))))
      (lambda (string &optional _pred &rest _unused)
        "Grep in the current directory for STRING."
        (if (< (length string) 3)
            (counsel-more-chars 3)
          (let* ((default-directory (ivy-state-directory ivy-last))
                 (args (if (string-match-p " -- " string)
                           (let ((split (split-string string " -- ")))
                             (prog1 (pop split)
                               (setq string (mapconcat #'identity split " -- "))))
                         ""))
                 (regex (counsel-unquote-regex-parens
                         (setq ivy--old-re
                               (ivy--regex string)))))
            (setq spacemacs--counsel-search-cmd (format base-cmd args regex))
            (spacemacs//counsel-async-command spacemacs--counsel-search-cmd)
            nil)))))

  (defun spacemacs//counsel-occur ()
    "Generate a custom occur buffer for `counsel-git-grep'."
    (ivy-occur-grep-mode)
    (setq default-directory (ivy-state-directory ivy-last))
    (let ((cands ivy--old-cands))
      ;; Need precise number of header lines for `wgrep' to work.
      (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                      default-directory))
      (insert (format "%d candidates:\n" (length cands)))
      (ivy--occur-insert-lines
       (mapcar
        (lambda (cand) (concat "./" cand))
        ivy--old-cands))))

  (defun spacemacs//counsel-with-git-grep (func x)
    (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
      (with-ivy-window
        (let ((file-name (match-string-no-properties 1 x))
              (line-number (match-string-no-properties 2 x)))
          (funcall func
                   (expand-file-name file-name (ivy-state-directory ivy-last)))
          (goto-char (point-min))
          (forward-line (1- (string-to-number line-number)))
          (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
          (unless (eq ivy-exit 'done)
            (swiper--cleanup)
            (swiper--add-overlays (ivy--regex ivy-text))))))))
