(require 'yxl-frame)
(require 'yxl-window)
(require 'yxl-buffer)
(require 'yxl-find)

(defvar yxl/new-buffer-inherit-white-list '(latex-mode
                                            gfm-mode
                                            org-mode
                                            R-mode
                                            python-mode
                                            emacs-lisp-mode)
  "modes that are allowed when calling yxl/new-buffer-inherit")
(defvar yxl/new-buffer-inherit-special-cases '((ess-mode . R-mode)
                                               (inferior-ess-mode . R-mode)))

(defvar yxl-switch-theme-hook nil
  "hook when yxl/switch-theme-light and yxl/swith-theme-dark invokes")



(defun yxl//translate-major-mode ()
  "Check if current `major-mode' is in `yxl/new-buffer-inherit-special-cases',
if true use the translated major-mode, else use original major-mode."
  ;; a more "lispier" implementation:
  ;; http://ergoemacs.org/emacs/elisp_break_loop.html
  ;; (catch 'translate-mode
  ;;   (mapc (lambda (x)
  ;;           (when (equal curr-mode (car x))
  ;;             (throw 'translate-mode (cdr x))))
  ;;         yxl/new-buffer-inherit-special-cases)
  ;;   nil)
  ;; but the one using loop is cleaner
  (let* ((curr-mode major-mode)
         (curr-mode-sp (loop for (key . value)
                             in yxl/new-buffer-inherit-special-cases
                             when (equal curr-mode key)
                             return value)))
         (if (not (eq curr-mode-sp nil))
             curr-mode-sp
           curr-mode)))

(defun yxl/new-buffer-inherit ()
  "Create a new buffer \"untitled\" which inherits the major-mode
of the previous buffer, if the major-mode is listed in
`yxl/new-buffer-inherit-white-list'; otherwise use `initial-major-mode'."
  (interactive)
  (let* ((curr-mode (yxl//translate-major-mode))
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

(defun yxl/show-and-copy-buffer-filename-in-projectile ()
  "TODO: document"
  (interactive)
  (let* ((file-name (or (buffer-file-name) list-buffers-directory))
         (proj-root (projectile-project-root))
         (relative-file-name (string-remove-prefix proj-root file-name)))
    (if relative-file-name
        (message (kill-new relative-file-name))
      (error "Buffer not visiting a file"))))



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
