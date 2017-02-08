(setq yxl-completion-packages '(helm
                                (yxl-helm-hotspot :location site)
                                ivy
                                counsel
                                (yxl-helm-pdf-occur :location site)
                                helm-github-stars))

(defun yxl-completion/post-init-helm ()
  (with-eval-after-load 'helm
    (setq helm-mini-default-sources '(helm-source-buffers-list))
    (setq helm-move-to-line-cycle-in-source t)
    ;; NOTE: these not work in terminal; Use C-o
    (define-key helm-map (kbd "C-S-k") #'helm-previous-source)
    (define-key helm-map (kbd "C-S-j") #'helm-next-source)
    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer(s) in new window(s) `M-v'" .
                   helm-buffer-switch-new-window-v) 'append)
    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer(s) in new window(s) `M-s'" .
                   helm-buffer-switch-new-window-s) 'append)
    (define-key helm-buffer-map (kbd "M-v") #'helm-buffer-switch-new-window-v)
    (define-key helm-buffer-map (kbd "M-s") #'helm-buffer-switch-new-window-h)))

(defun yxl-completion/init-yxl-helm-hotspot ()
  (use-package yxl-helm-hotspot
    :after 'helm
    :commands (yxl-helm-hotspot
               yxl-helm-shorcuts
               yxl-helm-org-files
               yxl-helm-reading-list)
    :defer t))

(defun yxl-completion/post-init-ivy ()
  ;; remove the "^" element in search
  (delete '(counsel-M-x . "^") ivy-initial-inputs-alist)
  (delete '(counsel-describe-function . "^") ivy-initial-inputs-alist)
  (delete '(counsel-describe-variable . "^") ivy-initial-inputs-alist)
  ;; ivy views
  (with-eval-after-load 'ivy
    (setq ivy-wrap t)
    (with-eval-after-load 'recentf
      (setq ivy-use-virtual-buffers nil))
    (load (concat dotspacemacs-directory "config/yxl-ivy-views.el"))))

(defun yxl-completion/post-init-counsel ()
  (with-eval-after-load 'counsel
    (define-key counsel-find-file-map (kbd "C-h") (kbd "DEL"))))

(defun yxl-completion/init-yxl-helm-pdf-occur ()
  (use-package yxl-helm-pdf-occur
    :after (helm pdf-tools)
    :commands (yxl-helm-pdf-occur yxl-pdf-occur-all-keywords)
    :defer t))

(defun yxl-completion/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "YiLiu6240"))))
