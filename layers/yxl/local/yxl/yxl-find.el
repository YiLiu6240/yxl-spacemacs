(defvar yxl-popwin-width-big 60)
(defvar yxl-popwin-width-small 40)


;; find file functions

(defun yxl-find-file-popup (file)
  (interactive)
  (let ((pop-width (yxl-window-popwin-width)))
    (cond
     ((equal current-prefix-arg '(4))
      (find-file file))
     ((equal current-prefix-arg 2)
      (find-file-other-window file))
     (t
      (yxl-window-popwin (find-file-noselect file) pop-width 'left)))))

(defun yxl-find-file-bib ()
  (interactive)
  (yxl-find-file-popup yxl-file-bib))

(defun yxl-find-file-org-popup ()
  (interactive)
  (let ((my-file yxl-file-org-main))
    (yxl-find-file-popup my-file)))

(defun yxl-find-file-org-work-popup ()
  (interactive)
  (let ((my-file yxl-file-org-work))
    (yxl-find-file-popup my-file)))

(defun yxl-find-file-org-config-popup ()
  (interactive)
  (let ((my-file yxl-file-org-config))
    (yxl-find-file-popup my-file)))

(defun yxl-find-file-org-proj-popup ()
  (interactive)
  (let ((my-file yxl-file-org-proj))
    (yxl-find-file-popup my-file)))

(defun yxl-find-file-org-scratch-popup ()
  (interactive)
  (let ((my-file yxl-file-org-scratch))
    (yxl-find-file-popup my-file)))

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

(defun yxl-find-file-open-all (file-list)
  "TODO: add doc"
  (let* ((file-len (length file-list))
         (action-list1 '(split-window-right-and-focus
                         (lambda ()
                           (split-window-below-and-focus)
                           (evil-window-move-very-bottom))))
         (action-list2 (mapcar (lambda (x)
                                 (if (/= (% x 2) 0)
                                     (car action-list1)
                                   (car (last action-list1))))
                               (number-sequence 1 (- file-len 1))))
         (action-list3 (cons nil action-list2))
         (final-alist (mapcar* 'cons file-list action-list3)))
    (delete-other-windows)
    (mapc (lambda (x)
            (when (cdr x)
              (funcall (cdr x)))
            (find-file (car x)))
          final-alist)))

(provide 'yxl-find)
