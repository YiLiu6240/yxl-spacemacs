(require 'evil)

(defun yth--update-evil-insert-state-cursor ()
  "Change terminal cursor to be IBeam."
  (unless (display-graphic-p)
    (send-string-to-terminal "\033[5 q")))

(defun yth--update-evil-normal-state-cursor ()
  "Change terminal cursor to be block."
  (unless (display-graphic-p)
    (send-string-to-terminal "\033[0 q")))

(defun yth--update-window-name (window &optional norecord)
  (unless (display-graphic-p)
    (let* ((buf-name (buffer-name))
           (buf-name-length (length buf-name))
           (max-length 20)
           (buf-name-new (if (> buf-name-length max-length)
                             (concat (substring buf-name 0 12)
                                     "..."
                                     (substring buf-name -5 nil))
                           buf-name)))
      (send-string-to-terminal (format "\ek%s\e\\" buf-name-new)))))

(defun yxl-tmux-helper-setup ()
  "1. Advice select-window, so that when current window changes
send string to update tmux window title.
2. Update terminal cursors based on evil states."
  (advice-add 'select-window :after #'yth--update-window-name)
  (add-hook 'evil-insert-state-entry-hook
            #'yth--update-evil-insert-state-cursor)
  (add-hook 'evil-normal-state-entry-hook
            #'yth--update-evil-normal-state-cursor))

(provide 'yxl-tmux-helper)
