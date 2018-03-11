(require 'org)
(require 'org-agenda)
(require 'noflet)

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

(defun make-capture-frame (&optional keys)
  "Create a new frame and run org-capture.

Source: https://stackoverflow.com/questions/15253005/in-emacs-org-mode-how-do-i-get-org-capture-to-open-in-a-full-sized-window"
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture nil keys)))

(defun hot-expand (str &optional mod header)
  "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
  (let (text)
    (when (region-active-p)
      (setq text (buffer-substring (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end))
      (deactivate-mark))
    (when header (insert "#+HEADER: " header) (forward-line))
    (insert str)
    (org-try-structure-completion)
    (when mod (insert mod) (forward-line))
    (when text (insert (string-trim text)))))

(defun org-toggle-link-display ()
  "Toggle the literal or descriptive display of links.
Source: https://emacs.stackexchange.com/questions/5387/show-org-mode-hyperlink-as-plain-text/5390"
  (interactive)
  (if org-descriptive-links
      (progn (org-remove-from-invisibility-spec '(org-link))
             (org-restart-font-lock)
             (setq org-descriptive-links nil))
    (progn (add-to-invisibility-spec '(org-link))
           (org-restart-font-lock)
           (setq org-descriptive-links t))))

(defun yxl-org-refile-region-to-heading (&optional heading)
  "Move current region to a user-selected heading or programmatically to HEADING represented by a marker.
Source: https://emacs.stackexchange.com/questions/36482/refile-selected-text-to-bottom-of-header-org-mode"
  (interactive)
  (let* ((heading (org-refile-get-location "Move region to: "))
         (target-pos (nth 3 heading))
         (target-buf (find-file-noselect (nth 1 heading))))
    (atomic-change-group
      (kill-region (region-beginning) (region-end))
      (with-current-buffer target-buf
        (goto-char target-pos)
        (org-back-to-heading t)
        (outline-next-heading)
        (yank)))))

(provide 'org-goodies)
