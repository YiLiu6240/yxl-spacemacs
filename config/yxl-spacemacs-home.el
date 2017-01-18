(defun spacemacs-buffer//insert-buttons ()
  (goto-char (point-max))
  (widget-create 'url-link
                 :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
                 :help-echo "Open the Spacemacs Github page in your browser."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "http://spacemacs.org")
  (insert " ")
  (widget-create 'push-button
                 :help-echo "Update Spacemacs core and layers."
                 :action (lambda (&rest ignore) (spacemacs/switch-to-version))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Update Spacemacs" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create 'push-button
                 :help-echo "Update all ELPA packages to the latest versions."
                 :action (lambda (&rest ignore)
                           (configuration-layer/update-packages))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Update Packages" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create 'push-button
                 :help-echo
                 "Rollback ELPA package updates if something got borked."
                 :action (lambda (&rest ignore)
                           (call-interactively 'configuration-layer/rollback))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Rollback Package Update"
                             'face 'font-lock-keyword-face))
  (insert "\n\n")
  (widget-create 'push-button
                 :tag (propertize "Agenda: work" 'face 'font-lock-type-face)
                 :action (lambda (&rest ignore) (yxl-org/agenda-work))
                 :mouse-face 'highlight)
  (insert " ")
  (widget-create 'push-button
                 :tag (propertize "Agenda: life" 'face 'font-lock-string-face)
                 :action (lambda (&rest ignore) (yxl-org/agenda-life))
                 :mouse-face 'highlight)
  (insert " ")
  (widget-create 'push-button
                 :tag (propertize "Org: work" 'face 'font-lock-type-face)
                 :action (lambda (&rest ignore) (yxl-find-file-org-work))
                 :mouse-face 'highlight)
  (insert " ")
  (widget-create 'push-button
                 :tag (propertize "Org: life" 'face 'font-lock-string-face)
                 :action (lambda (&rest ignore) (yxl-find-file-org))
                 :mouse-face 'highlight)
  (insert " ")
  (widget-create 'push-button
                 :tag (propertize "agenda-view" 'face 'font-lock-warning-face)
                 :action (lambda (&rest ignore) (yxl-org/agenda-view))
                 :mouse-face 'highlight)
  (insert "\n\n"))

(defun spacemacs-buffer//insert-footer ())

(defun configuration-layer/display-summary (start-time)
  "Display a summary of loading time."
  (unless configuration-layer--spacemacs-startup-time
    (setq configuration-layer--spacemacs-startup-time
          (float-time (time-subtract (current-time) emacs-start-time))))
  (let ((stats (configuration-layer/configured-packages-stats
                configuration-layer--used-packages)))
    (spacemacs-buffer/append
     (format "\n%s packages loaded in %.3fs (e:%s r:%s l:%s b:%s)"
             (cadr (assq 'total stats))
             configuration-layer--spacemacs-startup-time
             (cadr (assq 'elpa stats))
             (cadr (assq 'recipe stats))
             (cadr (assq 'local stats))
             (cadr (assq 'built-in stats))))
    (with-current-buffer (get-buffer-create spacemacs-buffer-name)
      (let ((buffer-read-only nil))
        (insert "\n")))))
