(setq yxl-workspace-packages '(eyebrowse))

(defun yxl-workspace/post-init-eyebrowse ()
  (with-eval-after-load 'eyebrowse

    (add-to-list 'window-persistent-parameters '(window-side . writable))
    (add-to-list 'window-persistent-parameters '(window-slot . writable))
    (setq eyebrowse-new-workspace 'spacemacs/home)
    ))
