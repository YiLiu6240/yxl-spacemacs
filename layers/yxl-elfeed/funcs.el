(defun yxl-elfeed/invoke-elfeed ()
  "When invoke with prefix arg, fetch news after invoking elfeed;
otherwise invoke elfeed as usual."
  (interactive)
  (if current-prefix-arg
      (progn
        (elfeed)
        (elfeed-update))
    (elfeed)))

(defun yxl-elfeed/elfeed-search-mode-hook-config ()
  (setq shr-inhibit-images t)
  (setq line-spacing 4))

(defun yxl-elfeed/elfeed-show-mode-hook-config ()
  (setq shr-inhibit-images t)
  (setq line-spacing 4)
  (setq visual-fill-column-width 120)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode t)
  (visual-line-mode t))

(defun yxl-elfeed/elfeed-search-browse-url-w3m ()
  "inject w3m to be the browser function"
  (interactive)
  (call-interactively #'elfeed-search-show-entry)
  (switch-to-buffer "*elfeed-entry*")
  (yxl-web/elfeed-show-visit-w3m)
  (yxl-web/elfeed-w3m-mode))

(defun yxl-elfeed/show-visit-w3m ()
  "inject w3m to be the browser function"
  (interactive)
  (let* ((browse-url-browser-function #'w3m-goto-url-new-session))
    (elfeed-show-visit)
    (yxl-elfeed/w3m-elfeed-mode)))

(define-minor-mode yxl-elfeed/w3m-elfeed-mode
  "For the w3m buffers invoked by elfeed, change its behaviour:
- q now quits window, rather than kills window."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "q" #'delete-window)
            map))

(defun yxl-elfeed/show-tag-unread ()
  (interactive)
  (elfeed-show-tag 'unread))

(defun yxl-elfeed/default-tag-filter ()
  "Restore the default tag filter state."
  (interactive)
  (elfeed--read-tag (default-value 'elfeed-search-filter)))

(defun yxl-elfeed/view-elfeed-log ()
  "Open log buffer."
  (interactive)
  (pop-to-buffer "*elfeed-log*"))
