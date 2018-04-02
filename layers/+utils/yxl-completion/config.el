(defun yxl-completion//setup-company ()
  (setq-default company-selection-wrap-around t)
  ;; Bind C-w to backward delete word, and rebind C-M-w to company-show-location
  (define-key company-active-map (kbd "C-w") #'evil-delete-backward-word)
  (define-key company-active-map (kbd "C-M-w") #'company-show-location)
  (define-key company-active-map (kbd "C-h") (kbd "DEL"))
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-l") #'company-complete-selection))

(defun yxl-completion//setup-helm ()
  (setq helm-mini-default-sources '(helm-source-buffers-list))
  (setq helm-move-to-line-cycle-in-source t)
  ;; NOTE: these not work in terminal; Use C-o
  ;; rebind C-z to C-tab, easier to press
  (define-key helm-map (kbd "C-<tab>") #'helm-select-action)
  (define-key helm-map (kbd "C-z") nil)
  (define-key helm-map (kbd "C-w") #'spacemacs/backward-kill-word-or-region)
  (define-key helm-map (kbd "C-h") #'backward-delete-char)
  (define-key helm-map (kbd "C-S-k") #'helm-previous-source)
  (define-key helm-map (kbd "C-S-j") #'helm-next-source)
  (with-eval-after-load 'helm-buffer
    (define-key helm-buffer-map (kbd "M-v") #'helm-buffer-switch-new-window-v)
    (define-key helm-buffer-map (kbd "M-s") #'helm-buffer-switch-new-window-h)
    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer(s) in new window(s) `M-v'" .
                   helm-buffer-switch-new-window-v) 'append)
    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer(s) in new window(s) `M-s'" .
                   helm-buffer-switch-new-window-s) 'append))
  (with-eval-after-load 'helm-files
    (dolist (map (list helm-find-files-map helm-read-file-map))
      (define-key map (kbd "C-w") #'spacemacs/backward-kill-word-or-region)
      (define-key map (kbd "C-h") #'backward-delete-char))))

(defun yxl-completion//setup-ivy ()
  ;; C-' conflicts with eyebrowse
  (define-key ivy-minibuffer-map (kbd "C-v") 'ivy-avy)
  (setq ivy-height 30)
  ;; (setq ivy-re-builders-alist
  ;;       '((ivy-switch-buffer . ivy--regex-plus)
  ;;         (t . ivy--regex-fuzzy)))
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (with-eval-after-load 'recentf
    (setq ivy-use-virtual-buffers nil)))

(defun yxl-completion//setup-counsel ()
  (define-key counsel-find-file-map (kbd "C-h") (kbd "DEL"))
  (define-key counsel-find-file-map (kbd "C-w") (kbd "DEL"))
  (ivy-add-actions 'counsel-find-file
                   '(("x" yxl-open-file-external "open in external program")))
  (advice-add #'counsel-projectile-switch-project
              :override #'yxl-counsel-projectile-switch-project)
  (ivy-add-actions 'counsel-projectile-switch-project
                   '(("o" (lambda (dir)
                            (let ((projectile-switch-project-action
                                   (lambda () (find-file (projectile-project-root)))))
                              (projectile-switch-project-by-name dir arg)))
                      "open"))))

(defun yxl-completion//setup-counsel-projectile ()
  (ivy-add-actions 'counsel-projectile-find-file
                   '(("d" (lambda (x) (dired-jump nil (projectile-expand-root x)))
                      "directory")
                     ("x" (lambda (x) (yxl-open-file-external (projectile-expand-root x)))
                      "external"))))
