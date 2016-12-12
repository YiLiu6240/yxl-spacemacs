(defvar yxl-buffer-stored-name nil)

(defvar yxl-buffer-inherit-whitelist '(latex-mode
                                       markdown-mode
                                       org-mode
                                       R-mode
                                       python-mode
                                       emacs-lisp-mode)
  "modes that are allowed when calling yxl-buffer-inherit")
(defvar yxl-buffer-inherit-special-alist '((ess-mode . R-mode)
                                           (inferior-ess-mode . R-mode)))

(defvar yxl-buffer-stored-name nil)



(defun yxl-buffer--translate-major-mode ()
  "Check if current `major-mode' is in `yxl-buffer-inherit-special-alist',
if true use the translated major-mode, else use original major-mode."
  (let* ((curr-mode major-mode)
         (curr-mode-sp (cdr (assoc curr-mode
                                   yxl-buffer-inherit-special-alist))))
    (if curr-mode-sp curr-mode-sp
      curr-mode)))

(defun yxl-buffer-inherit ()
  "Create a new buffer \"untitled\" which inherits the major-mode
of the previous buffer, if the major-mode is listed in
`yxl-buffer-inherit-whitelist'; otherwise use `initial-major-mode'."
  (interactive)
  (let* ((curr-mode (yxl-buffer--translate-major-mode))
         (newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)
    (if (member curr-mode yxl-buffer-inherit-whitelist)
        (funcall curr-mode)
      (funcall initial-major-mode))))



(defun yxl-buffer-store-name ()
  (interactive)
  (setq yxl-buffer-stored-name (buffer-name))
  (message "stored buffer name: %s" yxl-buffer-stored-name))

(defun yxl-buffer-visit-stored-buffer ()
  (interactive)
  (set-window-buffer (selected-window) yxl-buffer-stored-name)
  (message "switch to stored buffer: %s" yxl-buffer-stored-name))

(provide 'yxl-buffer)
