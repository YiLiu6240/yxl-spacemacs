(defun yxl-prog/evil-wrap-line-f ()
  (interactive)
  (end-of-line)
  (call-interactively #'set-mark-command)
  (back-to-indentation)
  (evil-surround-region (region-beginning) (region-end) t ?f))

(defun yxl-prog/evil-wrap-line-f-print ()
  (interactive)
  (end-of-line)
  (call-interactively #'set-mark-command)
  (back-to-indentation)
  ;; NOTE: this is conditional on ?F is set to `yxl-evil/surround-function-print'
  (evil-surround-region (region-beginning) (region-end) t ?F))

(defun yxl-prog/evil-wrap-line-f-lisp (&optional fname)
  (interactive)
  (end-of-line)
  (call-interactively #'set-mark-command)
  (back-to-indentation)
  (let* ((f (or fname
                (read-from-minibuffer "Function name: " "")))
         (surround-f (lambda ()
                       (cons (format "(%s " (or f ""))
                             ")")))
         (evil-surround-pairs-alist `((?x . ,surround-f))))
    (evil-surround-region (region-beginning) (region-end) t ?x)))

(defun yxl-prog/evil-wrap-line-f-lisp-print ()
  (interactive)
  (yxl-prog/evil-wrap-line-f-lisp "println"))

(defun python-shell-send-region-or-line-and-step ()
  "When a region is selected, send region using `python-shell-send-region',
otherwise select the current line and send region. After that, deactivate
region selection and step one line."
  (interactive)
  (if (use-region-p)
      (progn
        (python-shell-send-region (region-beginning) (region-end))
        (deactivate-mark))
    (progn
      (save-excursion
        (end-of-line)
        (let ((end (point)))
          (beginning-of-line)
          (python-shell-send-region (point) end)))
      (forward-line 1))))

(defun python-shell-send-string-print (string &optional process msg)
  "Wrap STRING with print() before sending it to
`python-shell-send-string'."
  (interactive
   (list (read-string "Python command: ") nil t))
  (let ((wrapped-string (concat "print(" string ")")))
    (python-shell-send-string wrapped-string process msg)))

(defun sh-send-line-or-region (&optional step)
  ;; https://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point)))
    ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))))

(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))
