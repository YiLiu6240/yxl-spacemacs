;; NOTE: Substitute for the loading of counsel-projectile
;;       in spacemacs-layouts package
;; TODO: post-init-helm is not working
(setq yxl-completion-packages '(company
                                ivy
                                counsel
                                counsel-projectile
                                (yxl-helm-pdf-occur :location site)
                                (yxl-ivy-views :location site)
                                helm-github-stars))

(defun yxl-completion/post-init-company ()
  (with-eval-after-load 'company
    (yxl-completion//setup-company)))

(defun yxl-completion/post-init-ivy ()
  (with-eval-after-load 'ivy
    (yxl-completion//setup-ivy))
  ;; HACK: config helm after ivy
  (with-eval-after-load 'helm
    (yxl-completion//setup-helm)))

(defun yxl-completion/post-init-counsel ()
  (with-eval-after-load 'counsel
    (yxl-completion//setup-counsel)))

(defun yxl-completion/init-counsel-projectile ()
  (use-package counsel-projectile
    :defer t))

(defun yxl-completion/post-inist-counsel-projectile ()
  (with-eval-after-load 'counsel-projectile
    (yxl-completion//setup-counsel-projectile)))

(defun yxl-completion/init-yxl-ivy-views ()
  (use-package yxl-ivy-views
    :after (ivy)
    :defer t
    :commands (yxl-ivy-push-view
               yxl-ivy-views-load
               yxl-ivy-views-save
               yxl-ivy-views-switch)))

(defun yxl-completion/init-yxl-helm-pdf-occur ()
  (use-package yxl-helm-pdf-occur
    :after (helm pdf-tools)
    :commands (yxl-helm-pdf-occur yxl-pdf-occur-all-keywords)
    :defer t))

(defun yxl-completion/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t))
