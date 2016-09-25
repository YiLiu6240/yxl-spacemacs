;; TODO: refine these functions
(define-minor-mode pdf-view-darknight-minor-mode
  "Apply a color-filter appropriate for past midnight reading.
The colors are determined by the variable
`pdf-view-midnight-colors', which see. "

  nil " Mid" nil
  (pdf-util-assert-pdf-buffer)
  (let ((enable (lambda ()
                  (pdf-info-setoptions
                   :render/foreground "#839496"
                   :render/background "#182c33"
                   :render/usecolors t))))
    (cond
     (pdf-view-darknight-minor-mode
      (add-hook 'after-save-hook enable nil t)
      (add-hook 'after-revert-hook enable nil t)
      (funcall enable))
     (t
      (remove-hook 'after-save-hook enable t)
      (remove-hook 'after-revert-hook enable t)
      (pdf-info-setoptions :render/usecolors nil))))
  (pdf-cache-clear-images)
  (pdf-view-redisplay t))

(define-minor-mode pdf-view-midday-minor-mode
  "Apply a color-filter appropriate for past midnight reading.
The colors are determined by the variable
`pdf-view-midnight-colors', which see. "

  nil " Mid" nil
  (pdf-util-assert-pdf-buffer)
  (let ((enable (lambda ()
                  (pdf-info-setoptions
                   :render/foreground "#657b83"
                   :render/background "#fdf6e3"
                   :render/usecolors t))))
    (cond
     (pdf-view-midday-minor-mode
      (add-hook 'after-save-hook enable nil t)
      (add-hook 'after-revert-hook enable nil t)
      (funcall enable))
     (t
      (remove-hook 'after-save-hook enable t)
      (remove-hook 'after-revert-hook enable t)
      (pdf-info-setoptions :render/usecolors nil))))
  (pdf-cache-clear-images)
  (pdf-view-redisplay t))

(defun yxl/pdf-view-goto-page ()
  "vim-style wrapper for pdf-view-goto-page. accepts G or 5G."
  (interactive)
  (if current-prefix-arg
      (pdf-view-goto-page (prefix-numeric-value current-prefix-arg))
    (pdf-view-last-page)))

(defun yxl/pdf-view-goto-first-page ()
  "vim-style wrapper for pdf-view-goto-page. accepts gg or 5gg."
  (interactive)
  (if current-prefix-arg
      (pdf-view-goto-page (prefix-numeric-value current-prefix-arg))
    (pdf-view-first-page)))

(defun zilong/elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun yxl/elfeed-mark-as-read ()
  (interactive)
  (elfeed-search-untag-all 'unread))
(defun yxl/elfeed-mark-as-unread ()
  (interactive)
  (elfeed-search-tag-all 'unread))

(defun elfeed-toggle-shr-inhibit-images ()
  "toggle the value of shr-inhibit-images"
  (interactive)
  (if (equal shr-inhibit-images t)
      (setq shr-inhibit-images nil)
    (setq shr-inhibit-images t))
  (message "shr-inhibit-images: %s" shr-inhibit-images))
