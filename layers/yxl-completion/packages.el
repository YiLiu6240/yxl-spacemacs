(setq yxl-completion-packages '(helm
                                ivy
                                counsel
                                (helm-pdf-occur :location local)))

(defun yxl-completion/post-init-helm ()
  (with-eval-after-load 'helm
    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer(s) in new window(s) `M-v'" .
                   helm-buffer-switch-new-window-v) 'append)
    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer(s) in new window(s) `M-s'" .
                   helm-buffer-switch-new-window-s) 'append)
    (define-key helm-buffer-map (kbd "M-v") #'helm-buffer-switch-new-window-v)
    (define-key helm-buffer-map (kbd "M-s") #'helm-buffer-switch-new-window-h)))

(defun yxl-completion/post-init-ivy ()
  ;; remove the "^" element in search
  (delete '(counsel-M-x . "^") ivy-initial-inputs-alist)
  (delete '(counsel-describe-function . "^") ivy-initial-inputs-alist)
  (delete '(counsel-describe-variable . "^") ivy-initial-inputs-alist))

(defun yxl-completion/post-init-counsel ()
  (with-eval-after-load 'counsel
    (define-key counsel-find-file-map (kbd "C-h") (kbd "DEL"))))

(defun yxl-completion/init-helm-pdf-occur ()
  (use-package helm-pdf-occur
    :after (helm pdf-tools)
    :commands (helm-pdf-occur helm-pdf-occur-search-preset)
    :defer t))
