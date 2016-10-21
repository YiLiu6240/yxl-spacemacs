;; TODO: define a default location for helm-dash-docsets-path
(setq helm-dash-packages '(helm-dash))

(defun helm-dash/init-helm-dash ()
  (use-package helm-dash
    :defer t
    :init
    (spacemacs/set-leader-keys
      "dH" 'helm-dash-at-point
      "dh" 'helm-dash)
    :config
    (setq helm-dash-browser-func 'w3m-goto-url-new-session)
    (setq helm-dash-docsets-path (expand-file-name "~/Dropbox/dash-docsets"))
    (defun helm-dash/activate-package-docsets (path)
      "Add dash docsets from specified PATH."
      (setq helm-dash-common-docsets (helm-dash-installed-docsets))
      (message (format "activated %d docsets from: %s"
                       (length helm-dash-common-docsets) path)))
    (helm-dash/activate-package-docsets helm-dash-docsets-path)))
