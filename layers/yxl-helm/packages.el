(setq yxl-helm-packages '(helm
                          counsel))

(defun yxl-helm/post-init-helm ()
  (with-eval-after-load 'helm
    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer(s) in new window(s) `M-v'" .
                   helm-buffer-switch-new-window-v) 'append)
    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer(s) in new window(s) `M-s'" .
                   helm-buffer-switch-new-window-s) 'append)
    (define-key helm-buffer-map (kbd "M-v") #'helm-buffer-switch-new-window-v)
    (define-key helm-buffer-map (kbd "M-s") #'helm-buffer-switch-new-window-h)))

(defun yxl-helm/post-init-counsel ()
  (with-eval-after-load 'counsel
    (define-key counsel-find-file-map (kbd "C-h") 'backward-delete-char)))
