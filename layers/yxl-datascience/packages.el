(setq yxl-datascience-packages '((counsel-dash)
                                 (yxl-dash :location site)
                                 (yxl-doc-portal :location site)
                                 ess
                                 (yxl-ess :location site)
                                 (ess-goodies :location site)
                                 ess-view
                                 polymode
                                 python))

(defun yxl-datascience/init-counsel-dash ()
  (use-package counsel-dash
    :commands (helm-dash-installed-docsets)))

(defun yxl-datascience/init-yxl-dash ()
  (use-package yxl-dash
    :after counsel-dash
    :commands (yxl-dash-select-docset
               yxl-dash-search-docset-external-browser)
    :init
    (progn
      (spacemacs/set-leader-keys
        "dh" #'yxl-dash-search-docset
        "dH" #'yxl-dash-search-docset-external-browser))
    :config
    (progn
      (setq yxl-dash-docset-path "~/Dropbox/dash-docsets")
      (setq yxl-dash-browser-func 'w3m-goto-url-new-session)
      (setq counsel-dash-browser-func yxl-dash-browser-func)
      (yxl-dash-activate-package-docsets yxl-dash-docset-path))))

(defun yxl-datascience/init-yxl-doc-portal ()
  (use-package yxl-doc-portal
    :defer t
    :commands (yxl-doc-portal)
    :init
    (progn
      (spacemacs/set-leader-keys
        "dp" #'yxl-doc-portal))
    :config
    (add-to-list 'yxl-dp-docs '("R - quantreg" . "https://cran.r-project.org/web/packages/quantreg/quantreg.pdf"))
    (add-to-list 'yxl-dp-docs '("moab commands" . "http://docs.adaptivecomputing.com/torque/6-1-1/adminGuide/help.htm#topics/moabWorkloadManager/topics/moabCommands/a.gcommandoverview.html"))))

(defun yxl-datascience/post-init-ess ()
  (with-eval-after-load 'ess-site
    (advice-add 'ess-set-style
                :after #'yxl-datascience/ess-set-style-advice)
    (yxl-datascience/setup-generic)
    (yxl-datascience/setup-bindings)
    (yxl-datascience/setup-imenu)
    (yxl-datascience/setup-rdired)
    (yxl-datascience/setup-ess-help)
    (yxl-datascience/setup-lintr)
    (add-hook 'ess-mode-hook #'yxl-datascience/ess-hook)
    (add-hook 'R-mode-hook #'yxl-datascience/R-hook)
    (add-hook 'ess-mode-hook 'smartparens-mode)
    (add-hook 'ess-mode-hook 'fci-mode)
    (add-hook 'ess-mode-hook 'hl-todo-mode)
    (mapcar #'yxl-datascience/set-leader-keys
            '(ess-mode ess-julia-mode inferior-ess-mode))
    (mapcar #'yxl-datascience/declare-prefix
            '(ess-mode ess-julia-mode inferior-ess-mode))))

(defun yxl-datascience/init-yxl-ess ()
  (use-package yxl-ess
    :after ess-site))

(defun yxl-datascience/init-ess-goodies ()
  (use-package ess-goodies
    :after ess-site))

(defun yxl-datascience/init-ess-view ()
  (use-package ess-view
    :after ess-site))

(defun yxl-datascience/init-polymode ()
  (use-package polymode
    ;; :mode (("\\.Rmd"   . Rmd-mode))
    :init
    (progn
      ;; TODO: try to toggle Rmd-mode
      (defun Rmd-mode ()
        "ESS Markdown mode for Rmd files"
        (interactive)
        (require 'poly-R)
        (require 'poly-markdown)
        (R-mode)
        (poly-markdown+r-mode)))))

(defun yxl-datascience/post-init-python ()
  (with-eval-after-load 'python
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      ";" #'python-shell-send-string)))
