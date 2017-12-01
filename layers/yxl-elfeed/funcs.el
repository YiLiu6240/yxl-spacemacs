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
  (setq yxl-line-width 100)
  (visual-line-mode t))

(defun yxl-elfeed/elfeed-search-browse-url-w3m ()
  "inject w3m to be the browser function"
  (interactive)
  (call-interactively #'elfeed-search-show-entry)
  (switch-to-buffer "*elfeed-entry*")
  (yxl-web/elfeed-show-visit-w3m)
  (yxl-web/elfeed-w3m-mode))

(defun yxl-elfeed/elfeed-show-visit-w3m ()
  "inject w3m to be the browser function"
  (interactive)
  (let* ((browse-url-browser-function #'w3m-goto-url-new-session))
    (elfeed-show-visit)
    (yxl-web/elfeed-w3m-mode)))

(define-minor-mode yxl-elfeed/elfeed-w3m-mode
  "For the w3m buffers invoked by elfeed, change its behaviour:
- q now quits window, rather than kills window."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "q" #'delete-window)
            map))
