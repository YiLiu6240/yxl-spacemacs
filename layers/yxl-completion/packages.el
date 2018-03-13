(setq yxl-completion-packages '(company
                                helm
                                (yxl-helm-hotspot :location site)
                                ivy
                                counsel
                                ivy-rich
                                (yxl-helm-pdf-occur :location site)
                                (yxl-ivy-views :location site)
                                helm-github-stars
                                yasnippet-snippets))

(defun yxl-completion/post-init-company ()
  (with-eval-after-load 'company
    (setq-default company-selection-wrap-around t)
    ;; (define-key company-active-map (kbd "<ESC>") #'company-cancel)
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map (kbd "C-j") #'company-select-next)
    (define-key company-active-map (kbd "C-k") #'company-select-previous)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)
    (define-key company-active-map (kbd "C-l") #'company-complete-selection)))

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
               yxl-helm-shortcuts
               yxl-helm-org-files
               yxl-helm-reading-list)))

(defun yxl-completion/post-init-ivy ()
  ;; remove the "^" element in search
  (with-eval-after-load 'ivy
    ;; C-' conflicts with eyebrowse
    (define-key ivy-minibuffer-map (kbd "C-v") 'ivy-avy)
    (setq ivy-height 30)
    ;; (setq ivy-re-builders-alist
    ;;       '((ivy-switch-buffer . ivy--regex-plus)
    ;;         (t . ivy--regex-fuzzy)))
    (setq ivy-wrap t)
    ;; FIXME: ivy-count-format causing troubles with counsel-search
    ;; (setq ivy-count-format "(%d/%d) ")
    (with-eval-after-load 'recentf
      (setq ivy-use-virtual-buffers nil))))

(defun yxl-completion/post-init-counsel ()
  (with-eval-after-load 'counsel
    (define-key counsel-find-file-map (kbd "C-h") (kbd "DEL"))
    (define-key counsel-find-file-map (kbd "C-w") (kbd "DEL"))
    (ivy-add-actions 'counsel-find-file
                     '(("x" yxl-open-file-external "open in external program")))
    ;; TODO: remove this following Spacemacs updates
    (yxl-completion/counsel-patch))
  (with-eval-after-load 'counsel-projectile
    (ivy-add-actions 'counsel-projectile-find-file
                     '(("d" (lambda (x) (dired-jump nil (projectile-expand-root x)))
                        "directory")
                       ("x" (lambda (x) (yxl-open-file-external (projectile-expand-root x)))
                        "external")))
    (defun yxl-counsel-projectile-switch-project (&optional arg)
      (interactive "P")
      (ivy-read (projectile-prepend-project-name "Switch to project: ")
                projectile-known-projects
                :preselect (and (projectile-project-p)
                                (abbreviate-file-name (projectile-project-root)))
                :action (lambda (dir)
                          (let ((projectile-switch-project-action
                                 (lambda () (find-file (projectile-project-root)))))
                           (projectile-switch-project-by-name dir arg)))
                :require-match t
                :caller 'counsel-projectile-switch-project))
    (advice-add #'counsel-projectile-switch-project
                :override #'yxl-counsel-projectile-switch-project)
    (ivy-add-actions 'counsel-projectile-switch-project
                     '(("o" (lambda (dir)
                              (let ((projectile-switch-project-action
                                     (lambda () (find-file (projectile-project-root)))))
                                (projectile-switch-project-by-name dir arg)))
                        "open")))))

(defun yxl-completion/init-ivy-rich ()
  (use-package ivy-rich
    :after ivy
    :config
    (progn
      (ivy-set-display-transformer
       'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
      (setq ivy-rich-path-style 'abbrev))))

(defun yxl-completion/init-yxl-ivy-views ()
  (use-package yxl-ivy-views
    :after (ivy)
    :defer t
    :commands (yxl-ivy-push-view
               yxl-ivy-views-load
               yxl-ivy-views-save
               yxl-ivy-views-switch)
    :config
    (progn
      (setq yxl-ivy-views-storage-location yxl-file-ivy-views))))

(defun yxl-completion/init-yxl-helm-pdf-occur ()
  (use-package yxl-helm-pdf-occur
    :after (helm pdf-tools)
    :commands (yxl-helm-pdf-occur yxl-pdf-occur-all-keywords)
    :defer t))

(defun yxl-completion/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t))

(defun yxl-completion/init-yasnippet-snippets ()
  (use-package yasnippet-snippets
    :after (yasnippet)))
