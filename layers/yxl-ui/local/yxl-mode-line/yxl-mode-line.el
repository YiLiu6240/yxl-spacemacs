(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))

(defun window-number-mode-line ()
  "The current window number. Requires `window-numbering-mode' to be enabled."
  (when (bound-and-true-p window-numbering-mode)
    (let* ((num (window-numbering-get-number))
           (str (when num (int-to-string num))))
      (format "%s" str))))

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

;; (setq evil-normal-state-tag    (propertize "<N>" 'face '((:background "DarkGoldenrod2" :foreground "black")))
;;       evil-emacs-state-tag     (propertize "<E>" 'face '((:background "SkyBlue2" :foreground "black")))
;;       evil-insert-state-tag    (propertize "<I>" 'face '((:background "chartreuse3") :foreground "white"))
;;       evil-motion-state-tag    (propertize "<M>" 'face '((:background "plum3") :foreground "black"))
;;       evil-visual-state-tag    (propertize "<V>" 'face '((:background "gray" :foreground "black")))
;;       evil-lisp-state-tag      (propertize "<L>" 'face '((:background "HotPink1" :foreground "black")))
;;       evil-operator-state-tag  (propertize "<O>" 'face '((:background "purple"))))

(setq my-flycheck-mode-line
      '(:eval
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
          ((\` suspicious) '(propertize " ?" 'face 'warning)))))

(setq-default mode-line-misc-info
              (assq-delete-all 'which-func-mode mode-line-misc-info))

(declare-function pdf-view-current-page 'pdf-view)
(declare-function pdf-cache-number-of-pages 'pdf-view)

(defun spaceline--pdfview-page-number ()
  (format "(%d/%d)"
          ;; `pdf-view-current-page' is a macro in an optional dependency
          ;; any better solutions?
          (eval `(pdf-view-current-page))
          (pdf-cache-number-of-pages)))
(defun line-column ()
  "The current line and column numbers, or `(current page/number of pages)`
in pdf-view mode (enabled by the `pdf-tools' package)."
  (if (eq 'pdf-view-mode major-mode)
      (spaceline--pdfview-page-number)
    "%l:%2c"))

(setq-default mode-line-format
              (list
               " %1 "
               ;; window number
               '(:eval (propertize (window-number-mode-line)
                                   'face
                                   'font-lock-type-face))
               " %1"
               '(:eval (eyebrowse-mode-line-indicator))

               " ["
               ;; line and column
               '(:eval (line-column))
               "]"

               ;; modified state
               '(:eval (cond (buffer-read-only " R ")
                             ((buffer-modified-p)
                              (propertize " M " 'face 'font-lock-warning-face
                                          'help-echo "Buffer has been modified"))
                             (t " - ")))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (when overwrite-mode
                         (propertize "Ovr"
                                     'face 'font-lock-preprocessor-face
                                     'help-echo (concat "Buffer is in "
                                                        (if overwrite-mode
                                                            "overwrite"
                                                          "insert") " mode"))))

               ;; evil state
               '(:eval (when (powerline-selected-window-active)
                         evil-mode-line-tag))

               ;; anzu
               anzu--mode-line-format

               "%1 "
               my-flycheck-mode-line
               "%1 "

               ;; ;; minor modes
               ;; '(:eval (when (> (window-width) 90)
               ;;           minor-mode-alist))
               " "
               ;; git info
               '(:eval (when (and (powerline-selected-window-active)
                                  (> (window-width) 120))
                         `(vc-mode vc-mode)))

               " "

               ;; global-mode-string goes in mode-line-misc-info
               ;; '(:eval (when (> (window-width) 120)
               ;;           mode-line-misc-info))

               (mode-line-fill 'mode-line-inactive 30)

               ;; the current major mode for the buffer.
               '(:eval (propertize " %m " 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))

               "["
               ;; size
               ;; "%I"
               ;; location
               "%p"
               "|"
               '(:eval (zilongshanren/display-mode-indent-width))
               "|"
               '(:eval (when (> (window-width) 80)
                         (buffer-encoding-abbrev)))
               "] "
               mode-line-end-spaces))

(provide 'yxl-mode-line)
