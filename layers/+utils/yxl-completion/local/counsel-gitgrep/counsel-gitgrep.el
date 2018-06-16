(require 'counsel)

;; From http://blog.binchen.org/posts/hello-ivy-mode-bye-helm.html

(defun cgg--api (fn git-cmd hint open-another-window local)
  "Apply FN on the output lines of GIT-CMD.  HINT is hint when user input.
If OPEN-ANOTHER-WINDOW is true, open the file in another window.

If local is true, do not change current directory."
  (let* ((keyword (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (read-string (concat "Enter " hint " pattern: "))))
         (curr-dir default-directory)
         (default-directory (if local
                                curr-dir
                              (locate-dominating-file
                               default-directory ".git")))
         collection val lst)

    (setq collection (split-string (shell-command-to-string (format git-cmd keyword))
                                   "\n"
                                   t))
    (when (and collection (> (length collection) 0))
      (setq val (if (= 1 (length collection)) (car collection)
                  (ivy-read (format " matching \"%s\": " keyword) collection)))
      (funcall fn open-another-window val))))

(defun cgg--yank-line (unused-param val)
  (let ((lst (split-string val ":")) text-line)
    ;; the actual text line could contain ":"
    (setq text-line (replace-regexp-in-string
                     (format "^%s:%s:" (car lst) (nth 1 lst)) "" val))
    ;; trim the text line
    (setq text-line (replace-regexp-in-string
                     (rx (* (any " \t\n")) eos) "" text-line))
    (kill-new text-line)
    ;; TODO: pass this
    (if insert-line (insert text-line))
    (message "line from %s:%s => kill-ring" (car lst) (nth 1 lst))))

(defun counsel-gitgrep-yank-line (&optional insert-line)
  "Grep in the current git repository and yank the line.
If INSERT-LINE is not nil, insert the line grepped"
  (interactive "P")
  (cgg--api #'cgg--yank-line
            "git --no-pager grep --full-name -n --no-color -i -e \"%s\""
            "grep"
            nil nil))

(defun counsel-gitgrep-revlist-yank-line (&optional insert-line)
  "Grep from revlist. Default to current file."
  (interactive "P")
  (let* ((current-file-only (eq current-prefix-arg nil))
         ;; global search over entire repo
         ;; local search only the current file
         ;; NOTE: their output is different, global output contains file name
         (cmd-global (concat "git rev-list --all"
                             " | "
                             "xargs git --no-pager grep --no-color \"%s\""
                             " | "
                             "sed 's/^[^:]*://'"
                             " | "
                             "sort -u"))
         (cmd-local (concat "git rev-list --all"
                            " | "
                            "xargs git --no-pager grep --no-color \"%s\""
                            (format " -- %s"
                                    (file-name-nondirectory (buffer-file-name)))
                            " | "
                            "sed 's/^[^:]*://'"
                            " | "
                            "sort -u"))
         (cmd (if current-file-only cmd-local cmd-global)))
    (cgg--api #'cgg--yank-line
              cmd
              "grep"
              nil t)))
