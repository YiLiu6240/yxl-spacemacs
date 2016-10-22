(defvar yxl-popwin-width-big 60)
(defvar yxl-popwin-width-small 40)

(defun yxl/find-dir-dotfiles ()
  (interactive)
  (find-file yxl/dotfiles))
(defun yxl/find-dir-Downloads ()
  (interactive)
  (find-file yxl/Downloads))
(defun yxl/find-dir-Dropbox ()
  (interactive)
  (find-file yxl/Dropbox))
(defun yxl/find-dir-org ()
  (interactive)
  (find-file yxl/org-directory))
(defun yxl/find-pwd-code ()
  (interactive)
  (find-file yxl/code-pwd))
(defun yxl/find-pwd-paper ()
  (interactive)
  (find-file yxl/paper-pwd))
(defun yxl/find-pwd-journal ()
  (interactive)
  (find-file yxl/journal-pwd))

;; find file functions
(defun yxl/find-file-bib ()
  (interactive)
  (find-file yxl/file-bib))

(defun yxl/find-file-org ()
  (interactive)
  (find-file yxl/org-file-master))
(defun yxl/find-file-org-other-window ()
  (interactive)
  (find-file-other-window yxl/org-file-master))
(defun yxl/find-file-org-popup ()
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (popwin:popup-buffer (find-file-noselect yxl/org-file-master)
                          :width yxl-popwin-width-big
                          :position 'left :stick t))
    (t
     (popwin:popup-buffer (find-file-noselect yxl/org-file-master)
                          :width yxl-popwin-width-small
                          :position 'left :stick t)))))



(defun yxl/find-file-org-work ()
  (interactive)
  (find-file yxl/org-file-work))
(defun yxl/find-file-org-work-other-window ()
  (interactive)
  (find-file-other-window yxl/org-file-work))
(defun yxl/find-file-org-work-popup ()
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (popwin:popup-buffer (find-file-noselect yxl/org-file-work)
                          :width yxl-popwin-width-big
                          :position 'left :stick t))
    (t
     (popwin:popup-buffer (find-file-noselect yxl/org-file-work)
                          :width yxl-popwin-width-small
                          :position 'left :stick t)))))

(defun yxl/find-file-org-dotfile-popup ()
  (interactive
   (let ((dotfile-todo "~/dotfiles/TODOs.org"))
     (cond
      ((equal current-prefix-arg nil)
       (popwin:popup-buffer (find-file-noselect dotfile-todo)
                             :width yxl-popwin-width-big :position 'left :stick t))
      (t
       (popwin:popup-buffer (find-file-noselect dotfile-todo)
                            :width yxl-popwin-width-small :position 'left :stick t))))))

;; TODO: change to org version as in capture template
(defun yxl/find-file-diary ()
  (interactive)
  (find-file diary-file))

(defun yxl/find-file-note ()
  (interactive)
  (find-file yxl-text-note-file))

(defun yxl/find-file-note-master ()
  (interactive)
  (find-file yxl/file-note-master))

(provide 'yxl-find)
