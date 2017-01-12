(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun dired-stay-or-jump ()
  "Jump to dired if current buffer is not a dired buffer.
Useful when creating new window layout/config."
  (interactive)
  (let ((cur-buf (buffer-name (current-buffer))))
    (if (eq major-mode 'dired-mode)
        (switch-to-buffer cur-buf)
      (dired-jump))))

(defun dired-popup ()
  (interactive)
  (popwin:popup-buffer (find-file-noselect default-directory)
                       :width 40 :position 'left :stick t))

(defun round-nb-in-region ()
  "http://stackoverflow.com/questions/23636226/how-to-round-all-the-numbers-in-a-region"
  (interactive)
  (let ((round-format (read-string "enter format (%0.4f): ")))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char 1)
      (let ((case-fold-search nil))
        (while (search-forward-regexp "\\([0-9]+\\.[0-9]+\\)" nil t)
          (replace-match (format round-format
                                 (string-to-number (match-string 1)))))))))

(defun copy-to-clipboard ()
  "Copies selection to x-clipboard."
  (interactive)
  (let ((cli-cmd (cond ((eq system-type 'darwin) "pbcopy")
                       ((eq system-type 'gnu/linux) "xsel --clipboard --input"))))
   (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save))
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) cli-cmd)
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))))

(defun paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (let ((cli-cmd (cond ((eq system-type 'darwin) "pbpaste")
                       ((eq system-type 'gnu/linux) "xsel --clipboard --input"))))
   (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active"))
     ;; TODO: check if the evil package is used
     (if (eq evil-state 'normal)
         (evil-append 1))
    (insert (shell-command-to-string cli-cmd)))))

(provide 'goodies)
