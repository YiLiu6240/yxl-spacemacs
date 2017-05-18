;; basically a trimmed version of the stock dash layer
(setq yxl-dash-packages '((helm-dash :toggle
                                     (configuration-layer/package-usedp 'helm))
                          (counsel-dash :toggle
                                        (configuration-layer/package-usedp 'ivy))))

(defun yxl-dash/init-helm-dash ()
  (use-package helm-dash
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "dH" 'helm-dash-at-point
        "dh" 'helm-dash))
    :config
    (progn
      (setq helm-dash-browser-func 'w3m-goto-url-new-session)
      (yxl-dash//activate-package-docsets yxl-dash-docset-path))))

(defun yxl-dash/init-counsel-dash ()
  (use-package counsel-dash
    :defer t
    :commands (helm-dash-installed-docsets yxl-dash/select-docset)
    :init
    (progn
      (spacemacs/set-leader-keys
        "dh" 'yxl-dash/search-docset
        "dH" 'yxl-dash/search-docset-external-browser))
    :config
    (progn
      (setq counsel-dash-browser-func yxl-dash-browser-func)
      (yxl-dash//activate-package-docsets yxl-dash-docset-path))))
