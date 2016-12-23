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



(defun yxl-helm-find-org-files ()
  (interactive)
  (helm :sources '(yxl-helm-org-sources)
        :buffer "*helm org agenda*"))

(defun yxl-helm-set-simple-todo ()
  (interactive)
  (helm :sources '(yxl-helm-simple-todo-sources)
        :buffer "*helm yxl simple-todo*"))

(defun yxl-helm-quick ()
  (interactive)
  (helm :sources '(yxl-helm-quick-sources)
        :buffer "*helm yxl quick*"))
