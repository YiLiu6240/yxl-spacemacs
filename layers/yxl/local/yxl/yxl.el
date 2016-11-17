(require 'yxl-frame)
(require 'yxl-window)
(require 'yxl-buffer)
(require 'yxl-find)
(require 'yxl-org)

(defvar yxl-switch-theme-hook nil
  "hook when yxl-switch-theme-light and yxl-swith-theme-dark invokes")



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



(defun yxl-switch-theme-light ()
  (interactive)
  (load-theme 'yxl-light t)
  (setq frame-background-mode 'light)
  (run-hooks 'yxl-switch-theme-hook))

(defun yxl-switch-theme-dark ()
  (interactive)
  (load-theme 'yxl-dark t)
  (setq frame-background-mode 'dark)
  (run-hooks 'yxl-switch-theme-hook))



(provide 'yxl)
