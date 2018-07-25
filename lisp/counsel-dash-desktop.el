(require 'counsel-dash)
(require 'yxl-dash)

(defun counsel-dash-desktop ()
  "A slightly tweaked version of counsel-dash that will destroy
the frame after completion. Useful to invoke from a desktop env."
  (let* ((counsel-dash-browser-func 'browse-url)
         (counsel-dash-common-docsets
          (list (ivy-read "which docset to use: "
                          (helm-dash-installed-docsets)
                          :history 'yxl-dash-search-history
                          :preselect (if (listp yxl-dash-search-history)
                                         (car yxl-dash-search-history)
                                       yxl-dash-search-history)))))
    (progn
      (counsel-dash-reset-connections)
      (helm-dash-initialize-debugging-buffer)
      (helm-dash-create-buffer-connections)
      (helm-dash-create-common-connections)
      (let ((cb (current-buffer)))
        (ivy-read "Documentation for: "
                  #'(lambda (s &rest _)
                      (with-current-buffer cb (counsel-dash-collection s)))
                  :dynamic-collection t
                  :history 'counsel-dash-history-input
                  :action (lambda (s)
                            (-when-let (result (-drop 1 (-first (-compose (-partial 'string= s) 'car) counsel-dash--results)))
                              (helm-dash-browse-url result)))
                  :unwind (lambda ()
                            (delete-frame)
                            (other-window 1)))))))

(provide 'counsel-dash-desktop)
