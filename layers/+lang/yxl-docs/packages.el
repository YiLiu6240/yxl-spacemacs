(setq yxl-docs-packages '(counsel-dash
                                 (yxl-dash :location site)
                                 (yxl-doc-portal :location site)))

(defun yxl-docs/init-counsel-dash ()
  (use-package counsel-dash
    :commands (helm-dash-installed-docsets)))

(defun yxl-docs/init-yxl-dash ()
  (use-package yxl-dash
    :init
    (progn
      (spacemacs/set-leader-keys
        "dH" #'yxl-dash-search-docset
        "dh" #'yxl-dash-search-docset-default-browser
        "dm" #'yxl-dash-search-docset-chromium
        "df" #'yxl-dash-search-docset-firefox
        "dr" #'counsel-dash-reset-connections))
    :commands (yxl-dash-search-docset
               yxl-dash-search-docset-default-browser
               yxl-dash-search-docset-chromium
               yxl-dash-search-docset-firefox
               yxl-dash-search-docset-helm)
    :config
    (progn
      (require 'counsel-dash)
      (setq yxl-dash-docset-path yxl-docs-docset-path)
      (setq yxl-dash-browser-func 'w3m-goto-url-new-session)
      (setq counsel-dash-browser-func yxl-dash-browser-func)
      (yxl-dash-activate-package-docsets yxl-dash-docset-path))))

(defun yxl-docs/init-yxl-doc-portal ()
  (use-package yxl-doc-portal
    :defer t
    :commands (yxl-doc-portal)
    :init
    (progn
      (spacemacs/set-leader-keys
        "dp" #'yxl-doc-portal
        "dP" #'yxl-doc-portal-chromium))))
