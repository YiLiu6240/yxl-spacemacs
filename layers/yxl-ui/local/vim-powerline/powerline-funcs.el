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

(defun spaceline--column-number-at-pos (pos)
  "Column number at POS.  Analog to `line-number-at-pos'."
  (save-excursion (goto-char pos) (current-column)))

(defun selection-info ()
  "Information about the size of the current selection, when applicable.
Supports both Emacs and Evil cursor conventions."
  (when (or mark-active
            (and (bound-and-true-p evil-local-mode)
                 (eq 'visual evil-state)))
    (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
           (chars (- (1+ (region-end)) (region-beginning)))
           (cols (1+ (abs (- (spaceline--column-number-at-pos (region-end))
                             (spaceline--column-number-at-pos (region-beginning))))))
           (evil (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
           (rect (or (bound-and-true-p rectangle-mark-mode)
                     (and evil (eq 'block evil-visual-selection))))
           (multi-line (or (> lines 1) (and evil (eq 'line evil-visual-selection)))))
      (cond
       (rect (format "%d×%d block" lines (if evil cols (1- cols))))
       (multi-line (format "%d lines" lines))
       (t (format "%d chars" (if evil chars (1- chars))))))))

(defun powerline-get-eyebrowse-tag-current ()
  (when (bound-and-true-p eyebrowse-mode)
    (let* ((window-configs (eyebrowse--get 'window-configs))
           (curr-id (eyebrowse--get 'current-slot))
           (curr-pos (cl-position (assoc curr-id window-configs) window-configs))
           (curr-tag (when curr-id (nth 2 (assoc curr-id window-configs))))
           (curr-str (if (and curr-tag (< 0 (length curr-tag)))
                         (concat (int-to-string curr-id) ":" curr-tag)
                       (when curr-id (int-to-string curr-id)))))
      (propertize curr-str 'face 'eyebrowse-mode-line-active))))

(defun powerline-get-eyebrowse-tag ()
  (when (and (bound-and-true-p eyebrowse-mode))
    (let* ((left-delimiter (propertize eyebrowse-mode-line-left-delimiter
                                       'face 'eyebrowse-mode-line-delimiters))
           (right-delimiter (propertize eyebrowse-mode-line-right-delimiter
                                        'face 'eyebrowse-mode-line-delimiters))
           (window-configs (eyebrowse--get 'window-configs))
           (config-len (length window-configs))
           ;; curr
           (curr-id (eyebrowse--get 'current-slot))
           (curr-pos (cl-position (assoc curr-id window-configs) window-configs))
           (curr-tag (when curr-id (nth 2 (assoc curr-id window-configs))))
           (curr-str (if (and curr-tag (< 0 (length curr-tag)))
                         (concat (int-to-string curr-id) ":" curr-tag)
                       (when curr-id (int-to-string curr-id))))
           ;; prev
           (prev-pos (- curr-pos 1))
           (prev-id (nth 0 (nth prev-pos window-configs)))
           (prev-str (concat (int-to-string prev-id)
                             ":"
                             (nth 2 (nth prev-pos window-configs))
                             ","))
           ;; next
           (next-pos (+ curr-pos 1))
           (next-id (nth 0 (nth next-pos window-configs)))
           ;; NOTE:
           ;; when next-pos >= config-len, (nth next-pos window-configs)
           ;; will return nil, and (nth 2 nil) will break
           (next-str (when (< next-pos config-len)
                       (concat ","
                               (int-to-string next-id)
                               ":"
                               (nth 2 (nth next-pos window-configs))))))
      (concat left-delimiter
              (when (and (> config-len 3) (> curr-pos 1))
                (propertize "…" 'face 'eyebrowse-mode-line-inactive))
              (when (and (> config-len 1) (> curr-pos 0))
                (propertize prev-str 'face 'eyebrowse-mode-line-inactive))
              (propertize curr-str 'face 'eyebrowse-mode-line-active)
              (when (and (> config-len 1) (< curr-pos (- config-len 1)))
                (propertize next-str 'face 'eyebrowse-mode-line-inactive))
              (when (and (> config-len 3) (< curr-pos (- config-len 2)))
                (propertize "…" 'face 'eyebrowse-mode-line-inactive))
              right-delimiter))))

(defun powerline-get-state-symbol (state)
  (let ((evil-symbol-alist '(("normal" . "N")
                             ("insert" . "I")
                             ("visual" . "V")
                             ("select" . "S")
                             ("replace" . "R")
                             ("motion" . "M")
                             ("emacs" . "e")
                             ("evilified" . "E")
                             ("iedit" . "Ie")
                             ("lisp" . "L")
                             ("inactive" . "N"))))
    (cdr (assoc state evil-symbol-alist))))

(provide 'powerline-funcs)
