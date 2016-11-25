(defvar yxl-popwin-width-big 60)
(defvar yxl-popwin-width-small 40)

(defun yxl-find-dir-dotfiles ()
  (interactive)
  (find-file yxl-path-dotfiles))
(defun yxl-find-dir-Downloads ()
  (interactive)
  (find-file yxl-path-local))
(defun yxl-find-dir-Dropbox ()
  (interactive)
  (find-file yxl-path-sync))
(defun yxl-find-dir-org ()
  (interactive)
  (find-file yxl-path-org))
(defun yxl-find-pwd-code ()
  (interactive)
  (find-file yxl-path-code-pwd))
(defun yxl-find-pwd-paper ()
  (interactive)
  (find-file yxl-path-paper-pwd))
(defun yxl-find-pwd-journal ()
  (interactive)
  (find-file yxl-path-journal-pwd))

;; find file functions
(defun yxl-find-file-bib ()
  (interactive)
  (find-file yxl-file-bib))

(defun yxl-find-file-org-popup ()
  (interactive)
  (let ((pop-width (yxl-window-popwin-width))
        (my-file yxl-file-org-main))
    (cond
     ((equal current-prefix-arg '(4))
      (find-file my-file))
     ((equal current-prefix-arg 2)
      (find-file-other-window my-file))
     (t
      (popwin:popup-buffer (find-file-noselect my-file)
                           :width pop-width :position 'left :stick t)))))

(defun yxl-find-file-org-work-popup ()
  (interactive)
  (let ((pop-width (yxl-window-popwin-width))
        (my-file yxl-file-org-work))
    (cond
     ((equal current-prefix-arg '(4))
      (find-file my-file))
     ((equal current-prefix-arg 2)
      (find-file-other-window my-file))
     (t
      (popwin:popup-buffer (find-file-noselect my-file)
                           :width pop-width :position 'left :stick t)))))

(defun yxl-find-file-org-dotfile-popup ()
  (interactive)
  (let ((pop-width (yxl-window-popwin-width))
        (my-file yxl-file-org-config))
    (cond
     ((equal current-prefix-arg '(4))
      (find-file my-file))
     ((equal current-prefix-arg 2)
      (find-file-other-window my-file))
     (t
      (popwin:popup-buffer (find-file-noselect my-file)
                           :width pop-width :position 'left :stick t)))))


(defun yxl-find-file-org-scratch-popup ()
  (interactive)
  ;; TODO: add pop-height as well
  (let ((pop-width (yxl-window-popwin-width))
        (my-file yxl-file-org-scratch))
    (cond
     ((equal current-prefix-arg '(4))
      (find-file my-file))
     ((equal current-prefix-arg 2)
      (popwin:popup-buffer (find-file-noselect my-file)
                           :width pop-width :position 'bottom :stick t))
     (t
      (popwin:popup-buffer (find-file-noselect my-file)
                           :width pop-width :position 'left :stick t)))))

;; TODO: change to org version as in capture template
(defun yxl-find-file-diary ()
  (interactive)
  (find-file diary-file))

(defun yxl-find-file-note ()
  (interactive)
  (find-file yxl-text-note-file))

(defun yxl-find-file-note-master ()
  (interactive)
  (find-file yxl-file-note-master))

(provide 'yxl-find)
