(require 'org)
(require 'org-agenda)

(defvar org-goodies-agenda-file "~/.org-agenda"
  "File to which we export agenda")

(defun org-goodies-export-agenda (&optional agenda-file agenda-args)
  "
Export agenda to a file.

Source: https://www.reddit.com/r/emacs/comments/7mec6e/orgmode_view_agenda_in_desktop_without_opening/"
  (interactive)
  (let* ((org-agenda-window-setup 'nope)
         (wins (current-window-configuration))
         (org-agenda-sticky nil)
         (file (or agenda-file org-goodies-agenda-file))
         (args (or agenda-args '(nil "a"))))
    (save-excursion
      (with-current-buffer
          (get-buffer-create org-agenda-buffer-name)
        (pop-to-buffer (current-buffer))
        (apply #'org-agenda args)
        (let ((result (buffer-string)))
          (with-temp-file file (insert result)))))
    (set-window-configuration wins)))

(provide 'org-goodies)
