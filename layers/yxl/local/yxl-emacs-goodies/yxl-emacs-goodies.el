(require 'yxl-frame)
(require 'yxl-window)
(require 'yxl-buffer)
(require 'yxl-find)
(require 'yxl-org)
(require 'yxl-web)



(defun yxl-append-to-scratch (text)
  "receive input text and append this text to scratch"
  (interactive "stext: ")
  (let* ((scratch-buf (get-buffer-create "*scratch*"))
         (text-with-newline (concat text "\n")))
    (save-excursion
      (with-current-buffer scratch-buf
        (end-of-buffer)
        (insert text-with-newline)))))

(defun yxl-show-and-copy-buffer-filename-in-projectile ()
  "TODO: document"
  (interactive)
  (let* ((file-name (or (buffer-file-name) list-buffers-directory))
         (proj-root (projectile-project-root))
         (relative-file-name (string-remove-prefix proj-root file-name)))
    (if relative-file-name
        (message (kill-new relative-file-name))
      (error "Buffer not visiting a file"))))



(define-minor-mode yxl-big-text-mode
  "Bigger text."
  :lighter ""
  (if yxl-big-text-mode
      (let ((scale 1.5))
        (text-scale-increase scale))
    (text-scale-set 0)))



(provide 'yxl-emacs-goodies)
