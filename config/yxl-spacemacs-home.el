(defun spacemacs-buffer//insert-buttons ()
  (goto-char (point-max))
  (widget-create 'push-button
                 :action (lambda (&rest ignore)
                           (yxl-hydra-hotspot/body))
                 :mouse-face 'highlight
                 (propertize "Hotspot" 'face 'font-lock-keyword-face))
  (insert "\n")
  (widget-create 'push-button
                 :help-echo "Update all ELPA packages to the latest versions."
                 :action (lambda (&rest ignore)
                           (yxl-hydra-system/body))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "System" 'face 'font-lock-keyword-face))
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

(defun spacemacs-buffer/display-info-box ()
  "Display an info box."
  (spacemacs//redisplay))
