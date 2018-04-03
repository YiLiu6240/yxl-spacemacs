(defun cfw/open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source (face-foreground 'org-level-3)))))

(defun cfw/most-recent-date ()
  (interactive)
  (format-time-string "%Y-%m-%d"
                      (cfw:calendar-to-emacs
                       (with-current-buffer (get-buffer cfw:calendar-buffer-name)
                         (cfw:cursor-to-nearest-date)))))
