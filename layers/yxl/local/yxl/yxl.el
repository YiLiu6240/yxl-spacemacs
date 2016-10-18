(require 'yxl-frame)
(require 'yxl-window)
(require 'yxl-find)

(defvar yxl/new-buffer-inherit-white-list '(latex-mode
                                            gfm-mode
                                            org-mode
                                            R-mode
                                            python-mode
                                            emacs-lisp-mode)
  "modes that are allowed when calling yxl/new-buffer-inherit")

(defvar yxl-switch-theme-hook nil
  "hook when yxl/switch-theme-light and yxl/swith-theme-dark invokes")

(defun yxl/new-buffer-inherit ()
  (interactive)
  (let* ((curr-mode major-mode)
         (newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)
    (if (member curr-mode yxl/new-buffer-inherit-white-list)
        (funcall curr-mode)
      (funcall initial-major-mode))))

(defun yxl/append-to-scratch (text)
  "receive input text and append this text to scratch"
  (interactive "stext: ")
  (let* ((scratch-buf (get-buffer-create "*scratch*"))
         (text-with-newline (concat text "\n")))
    (save-excursion
      (with-current-buffer scratch-buf
        (end-of-buffer)
        (insert text-with-newline)))))

(defun yxl/switch-theme-light ()
  (interactive)
  (load-theme 'yxl-light t)
  (setq frame-background-mode 'light)
  (run-hooks 'yxl-switch-theme-hook))

(defun yxl/switch-theme-dark ()
  (interactive)
  (load-theme 'yxl-dark t)
  (setq frame-background-mode 'dark)
  (run-hooks 'yxl-switch-theme-hook))

(provide 'yxl)
