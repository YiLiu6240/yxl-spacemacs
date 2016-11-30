;; minimal dekstop setup, from emacswiki

;; use only one desktop
(setq yxl-desktop-dirname "~/Dropbox/inbox/")
(setq desktop-base-file-name "yxl-emacs-desktop")
(setq desktop-file-name-format 'tilde)
(setq history-length 100)
(with-eval-after-load 'desktop
 (add-to-list 'desktop-modes-not-to-save 'image-mode)
 ;; (add-to-list 'desktop-modes-not-to-save 'pdf-view-mode)
 (add-to-list 'desktop-modes-not-to-save 'csv-mode)
 (add-to-list 'desktop-modes-not-to-save 'cfw:calendar-mode)
 (add-to-list 'desktop-modes-not-to-save 'elfeed-search-mode)
 (add-to-list 'desktop-modes-not-to-save 'elfeed-show-mode))

(defun yxl-saved-session-p ()
  (file-exists-p (concat yxl-desktop-dirname "/"
                         desktop-base-file-name)))

(defun yxl-session-load-1 ()
  "Restore a saved emacs session."
  (interactive)
  (let ((desktop-base-file-name "yxl-emacs-desktop-1"))
    (if (yxl-saved-session-p)
        (desktop-read yxl-desktop-dirname)
      (message "No desktop found."))))

(defun yxl-session-load-2 ()
  "Restore a saved emacs session."
  (interactive)
  (let ((desktop-base-file-name "yxl-emacs-desktop-2"))
    (if (yxl-saved-session-p)
        (desktop-read yxl-desktop-dirname)
      (message "No desktop found."))))

(defun yxl-session-save-1 ()
  "Save an emacs session."
  (interactive)
  (let ((desktop-base-file-name "yxl-emacs-desktop-1"))
    (if (yxl-saved-session-p)
        (if (y-or-n-p "Overwrite existing desktop? ")
            (desktop-save yxl-desktop-dirname)
          (message "Session not saved."))
      (desktop-save yxl-desktop-dirname))))

(defun yxl-session-save-2 ()
  "Save an emacs session."
  (interactive)
  (let ((desktop-base-file-name "yxl-emacs-desktop-2"))
    (if (yxl-saved-session-p)
        (if (y-or-n-p "Overwrite existing desktop? ")
            (desktop-save yxl-desktop-dirname)
          (message "Session not saved."))
      (desktop-save yxl-desktop-dirname))))
