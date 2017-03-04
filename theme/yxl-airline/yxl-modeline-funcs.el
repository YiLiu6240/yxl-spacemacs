(defun buffer-id-short (width)
  (let* ((buf-name-full (format-mode-line "%b"))
         (buf-name-length (length buf-name-full))
         (buf-name-new (if (> buf-name-length 33)
                           (concat (substring buf-name-full 0 20)
                                   "..."
                                   (substring buf-name-full -10 nil))
                         buf-name-full))
         (buf-out (if (< (window-width) width)
                      buf-name-new
                    buf-name-full)))
    buf-out))

(defun modeline-buffer-id-short (width &optional face pad)
  (powerline-raw
   (format-mode-line
    (concat " " (propertize
                 (buffer-id-short width)
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

(defun modeline-flycheck ()
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

(defun modeline-pdfview-page-number ()
  (format "(%d/%d)"
          ;; `pdf-view-current-page' is a macro in an optional dependency
          ;; any better solutions?
          (eval `(pdf-view-current-page))
          (pdf-cache-number-of-pages)))

(defun modeline--column-number-at-pos (pos)
  "Column number at POS.  Analog to `line-number-at-pos'."
  (save-excursion (goto-char pos) (current-column)))

(defun modeline-selection-info ()
  "Information about the size of the current selection, when applicable.
Supports both Emacs and Evil cursor conventions."
  (when (or mark-active
            (and (bound-and-true-p evil-local-mode)
                 (eq 'visual evil-state)))
    (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
           (chars (- (1+ (region-end)) (region-beginning)))
           (cols (1+ (abs (- (modeline--column-number-at-pos (region-end))
                             (modeline--column-number-at-pos (region-beginning))))))
           (evil (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
           (rect (or (bound-and-true-p rectangle-mark-mode)
                     (and evil (eq 'block evil-visual-selection))))
           (multi-line (or (> lines 1) (and evil (eq 'line evil-visual-selection)))))
      (cond
       (rect (format "%d×%d block" lines (if evil cols (1- cols))))
       (multi-line (format "%d lines" lines))
       (t (format "%d chars" (if evil chars (1- chars))))))))

(defun modeline-get-eyebrowse-tag-current ()
  (when (bound-and-true-p eyebrowse-mode)
    (let* ((window-configs (eyebrowse--get 'window-configs))
           (config-len (length window-configs))
           (curr-id (eyebrowse--get 'current-slot))
           (curr-pos (cl-position (assoc curr-id window-configs) window-configs))
           (curr-tag (when curr-id (nth 2 (assoc curr-id window-configs))))
           (curr-str (propertize (if (and curr-tag (< 0 (length curr-tag)))
                                     (concat (int-to-string curr-id) ":" curr-tag)
                                   (when curr-id (int-to-string curr-id)))
                                 'face 'eyebrowse-mode-line-active))
           (str (concat "[" curr-str "/" (int-to-string config-len) "]")))
      str)))

(defun modeline-get-state-symbol (state)
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

(defun modeline-get-vc ()
  (if (featurep 'magit)
      (magit-get-current-branch)
    " "))

(provide 'yxl-modeline-funcs)
