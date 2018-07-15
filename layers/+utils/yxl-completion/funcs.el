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

(defun yxl-counsel-projectile-switch-project (&optional arg)
  (interactive "P")
  (ivy-read (projectile-prepend-project-name "Switch to project: ")
            projectile-known-projects
            :preselect (and (projectile-project-p)
                            (abbreviate-file-name (projectile-project-root)))
            :action (lambda (dir)
                      (let ((projectile-switch-project-action
                             (lambda () (find-file (projectile-project-root)))))
                        (projectile-switch-project-by-name dir arg)))
            :require-match t
            :caller 'counsel-projectile-switch-project))

(defun yxl-completion/company-quickhelp-toggle ()
  "Toggle company quickhelp."
  (interactive)
  (or (x-hide-tip)
      (call-interactively #'company-quickhelp-manual-begin)))
