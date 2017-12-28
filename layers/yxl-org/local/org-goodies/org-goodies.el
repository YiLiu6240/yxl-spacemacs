(require 'org)
(require 'org-agenda)

(defvar org-goodies-agenda-file "~/.org-agenda"
  "File to which we export agenda")

(defun org-goodies-export-agenda ()
  "
Source: https://www.reddit.com/r/emacs/comments/7mec6e/orgmode_view_agenda_in_desktop_without_opening/"
  (interactive)
  (let* ((org-agenda-span 'day)
         (org-agenda-use-time-grid t)
         (org-agenda-remove-tags t)
         (org-agenda-window-setup 'nope)
         (wins (current-window-configuration))
         (org-agenda-sticky nil))
    (save-excursion
      (with-current-buffer
          (get-buffer-create org-agenda-buffer-name)
        (pop-to-buffer (current-buffer))
        (org-agenda nil "a")
        (let ((result (buffer-string)))
          (with-temp-file org-goodies-agenda-file (insert result)))))
    (set-window-configuration wins)))

(provide 'org-goodies)
