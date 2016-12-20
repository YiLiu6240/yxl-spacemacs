(require 'dired)

(defun dired-zip-files (zip-file)
  "Create an archive containing the marked files.
Alternatively, run \"! zip foo.zip * <RET>\" for marked files in dired.
source:
http://stackoverflow.com/questions/1431351/how-do-i-uncompress-unzip-within-emacs"
  (interactive "sEnter name of zip file: ")

  ;; create the zip file
  (let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
    (shell-command
     (concat "zip "
             zip-file
             " "
             (concat-string-list
              (mapcar
               '(lambda (filename)
                  (file-name-nondirectory filename))
               (dired-get-marked-files))))))

  (revert-buffer)

  ;; remove the mark on all the files  "*" to " "
  ;; (dired-change-marks 42 ?\040)
  ;; mark zip file
  ;; (dired-mark-files-regexp (filename-to-regexp zip-file))
  )

(defun concat-string-list (list)
  "Return a string which is a concatenation of all elements of the list separated by spaces"
  (mapconcat '(lambda (obj) (format "%s" obj)) list " "))

(defun ivy-get-dired-buffer-list ()
  (delq nil
        (mapcar
         (lambda (buffer)
           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
             (buffer-local-value 'default-directory buffer)))
         (buffer-list))))

(defun ivy-get-non-dired-buffer-list ()
  (delq nil
        (mapcar
         (lambda (buffer)
           (when (not (eq 'dired-mode (buffer-local-value 'major-mode buffer)))
             (buffer-file-name buffer)))
         (buffer-list))))

(defun ivy-switch-dired-buffer ()
  "Switch to another buffer."
  (interactive)
  (ivy-read "Switch to dired buffer: "
            (ivy-get-dired-buffer-list)
            :action (lambda (x)
                      (find-file x))
            :caller 'ivy-switch-dired-buffer))

(provide 'dired-goodies)
