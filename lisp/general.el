(defun yas-or-company ()
  (interactive)
  (if company-mode
      (call-interactively #'company-yasnippet)
    (yas-insert-snippet)))

(defun yxl-buffer-compilation ()
  (interactive)
  (switch-to-buffer "*compilation*"))

(defun yxl-buffer-messages ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*Messages*")))

(defun yxl-buffer-home-dir ()
  (interactive)
  (find-file "~"))

(defun yxl-buffer-org-checkbox ()
  (interactive)
  (find-file yxl-file-org-checkbox))

(defun yxl-buffer-sticky-checkbox ()
  "Visit a indirect buffer of the checkbox.
Useful when we need two instances (one sticky note, and
one normal checkbox buffer."
  (interactive)
  (let* ((checkbox-name (file-name-nondirectory yxl-file-org-checkbox))
         (sticky-name (concat "*sitcky*" checkbox-name))
         (checkbox-buf (find-file-noselect yxl-file-org-checkbox))
         (sticky-buf (get-buffer sticky-name)))
    (if sticky-buf
        (switch-to-buffer sticky-buf)
      (progn
        (switch-to-buffer (make-indirect-buffer checkbox-buf sticky-name t))
        (text-scale-set 0)
        (spacemacs/toggle-mode-line-off)))))

(defun yxl-buffer-org-todo ()
  (interactive)
  (find-file yxl-file-org-todo))

(defun yxl-buffer-org-log ()
  (interactive)
  (find-file yxl-file-org-log))

(defun yxl-buffer-note-local ()
  (interactive)
  (find-file yxl-file-note-local))

(defun yxl-buffer-note-sync ()
  (interactive)
  (find-file yxl-file-note-sync))

(defun visual-fill-column-toggle-center-text ()
  (interactive)
  (setq visual-fill-column-center-text
        (not visual-fill-column-center-text))
  (message (format "visual-fill-column-center-text: %s"
                   visual-fill-column-center-text)))

(defun spacemacs/find-dotfile-follow-symlink ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (file-truename (dotspacemacs/location))))

(defun yxl-ivy-find-project ()
  (interactive)
  (ivy-read "Switch to project: "
            yxl-env-projects
            ;; add a prefix-arg to visit directly
            :action #'yxl-find-dir))

(defun yxl-ivy-find-file ()
  (interactive)
  (ivy-read "Find file: "
            yxl-env-files
            :action #'yxl-find-file-stay))
