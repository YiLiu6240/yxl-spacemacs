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

;; remove desktop after it's been read
;; (add-hook 'desktop-after-read-hook
;;       '(lambda ()
;;          ;; desktop-remove clears desktop-dirname
;;          (setq desktop-dirname-tmp desktop-dirname)
;;          (desktop-remove)
;;          (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session-p ()
  (file-exists-p (concat yxl-desktop-dirname "/"
                         desktop-base-file-name)))

;; use session-load to restore the desktop manually
(defun session-load ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session-p)
      (desktop-read yxl-desktop-dirname)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session-p)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save yxl-desktop-dirname)
        (message "Session not saved."))
    (desktop-save yxl-desktop-dirname)))
