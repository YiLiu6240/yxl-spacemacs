(defun buffer-id-short ()
  (let* ((buf-name-full (format-mode-line "%b"))
         (buf-name-length (length buf-name-full))
         (buf-name-new (if (> buf-name-length 33)
                           (concat (substring buf-name-full 0 20)
                                   "..."
                                   (substring buf-name-full -10 nil))
                         buf-name-full)))
    buf-name-new))

(defun powerline-buffer-id-short (&optional face pad)
  (powerline-raw
   (format-mode-line
    (concat " " (propertize
                 (buffer-id-short)
                 'face face
                 'mouse-face 'mode-line-highlight
                 'help-echo (concat
                             (format-mode-line "%b")
                             "\n\ mouse-1: Previous buffer\n\ mouse-3: Next buffer")
                 'local-map (let ((map (make-sparse-keymap)))
                              (define-key map [mode-line mouse-1]
                                'mode-line-previous-buffer)
                              (define-key map [mode-line mouse-3]
                                'mode-line-next-buffer)
                              map))))
   face pad))

(defun my-flycheck ()
  (pcase flycheck-last-status-change
    ((\` not-checked) nil)
    ((\` no-checker) (propertize " -" 'face 'warning))
    ((\` running) (propertize " ✷" 'face 'success))
    ((\` errored) (propertize " !" 'face 'error))
    ((\` finished)
     (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
            (no-errors (cdr (assq 'error error-counts)))
            (no-warnings (cdr (assq 'warning error-counts)))
            (face (cond (no-errors 'error)
                        (no-warnings 'warning)
                        (t 'success))))
       (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
                   'face face)))
    ((\` interrupted) " -")
    ((\` suspicious) '(propertize " ?" 'face 'warning))))

(defun spaceline--pdfview-page-number ()
  (format "(%d/%d)"
          ;; `pdf-view-current-page' is a macro in an optional dependency
          ;; any better solutions?
          (eval `(pdf-view-current-page))
          (pdf-cache-number-of-pages)))

(defun zilongshanren/display-mode-indent-width ()
  (let ((mode-indent-level
         (catch 'break
           (dolist (test spacemacs--indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break evil-shift-width))))
    (concat "i:" (int-to-string (or mode-indent-level 0)))))

(provide 'powerline-funcs)
