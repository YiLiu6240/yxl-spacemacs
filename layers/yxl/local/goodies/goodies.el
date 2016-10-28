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

(provide 'goodies)
