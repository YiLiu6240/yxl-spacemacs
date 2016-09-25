(setq yxl-helm-packages '(helm))

(defun yxl-helm/post-init-helm ()
  (with-eval-after-load 'helm
    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer(s) in new window(s) `M-v'" .
                   helm-buffer-switch-new-window-v) 'append)
    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer(s) in new window(s) `M-s'" .
                   helm-buffer-switch-new-window-s) 'append)
    (define-key helm-buffer-map (kbd "M-v") #'helm-buffer-switch-new-window-v)
    (define-key helm-buffer-map (kbd "M-s") #'helm-buffer-switch-new-window-h))

  ;; rebind C-z to C-tab, easier to press
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-<tab>") 'helm-select-action)
    (define-key helm-map (kbd "C-h") 'backward-delete-char)
    ;; overwrite spacemacs default
    (define-key helm-find-files-map (kbd "C-h") 'backward-delete-char)
    (define-key helm-read-file-map (kbd "C-h") 'backward-delete-char)
    (define-key helm-map (kbd "C-z") nil)))
