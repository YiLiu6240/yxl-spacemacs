(setq yxl-workspace-packages '(eyebrowse))

(defun yxl-workspace/post-init-eyebrowse ()
  (with-eval-after-load 'eyebrowse
    (add-to-list 'window-persistent-parameters '(window-side . writable))
    (add-to-list 'window-persistent-parameters '(window-slot . writable))
    (setq eyebrowse-new-workspace 'spacemacs/home)
    ;; cut integration between persp and workspaces
    (remove-hook 'persp-before-switch-functions
              #'spacemacs/update-eyebrowse-for-perspective)
    (remove-hook 'eyebrowse-post-window-switch-hook
              #'spacemacs/save-eyebrowse-for-perspective)
    (remove-hook 'persp-activated-functions
              #'spacemacs/load-eyebrowse-for-perspective)
    (remove-hook 'persp-before-save-state-to-file-functions
                 #'spacemacs/update-eyebrowse-for-perspective)
    (remove-hook 'persp-after-load-state-functions
                 #'spacemacs/load-eyebrowse-after-loading-layout)))
