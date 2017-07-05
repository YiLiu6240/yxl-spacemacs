(setq yxl-datascience-packages '((counsel-dash)
                                 (yxl-dash :location site)
                                 (yxl-doc-portal :location site)
                                 ess
                                 (yxl-ess :location site)
                                 (ess-goodies :location site)
                                 ess-view
                                 markdown-mode
                                 org-mode
                                 polymode
                                 python))

(defun yxl-datascience/init-counsel-dash ()
  (use-package counsel-dash
    :commands (helm-dash-installed-docsets)))

(defun yxl-datascience/init-yxl-dash ()
  (use-package yxl-dash
    :after counsel-dash
    :commands (yxl-dash-search-docset
               yxl-dash-search-docset-external-browser)
    :init
    (progn
      (spacemacs/set-leader-keys
        "dh" #'yxl-dash-search-docset
        "dH" #'yxl-dash-search-docset-external-browser
        "dm" #'yxl-dash-search-docset-chromium))
    :config
    (progn
      (setq yxl-dash-docset-path "~/Dropbox/dash-docsets")
      (setq yxl-dash-browser-func 'w3m-goto-url-new-session)
      (setq counsel-dash-browser-func yxl-dash-browser-func)
      (yxl-dash-activate-package-docsets yxl-dash-docset-path)
      (defun yxl-dash-search-docset-chromium ()
        (interactive)
        (let ((counsel-dash-browser-func #'browse-url-chromium))
          (yxl-dash-search-docset))))))

(defun yxl-datascience/init-yxl-doc-portal ()
  (use-package yxl-doc-portal
    :defer t
    :commands (yxl-doc-portal)
    :init
    (progn
      (spacemacs/set-leader-keys
        "dp" #'yxl-doc-portal
        "dP" #'yxl-doc-portal-chromium))
    :config
    (progn
      (defun yxl-doc-portal-chromium ()
        (interactive)
        (let ((browse-url-browser-function #'browse-url-chromium))
          (yxl-doc-portal)))
      (setq yxl-dp-docs
            (delete-dups (sort (append yxl-dp-docs
                                       yxl-datascience-additional-docs)
                               (lambda (elem1 elem2)
                                 (let ((str1 (car elem1))
                                       (str2 (car elem2)))
                                   (string-lessp str1 str2)))))))))

(defun yxl-datascience/post-init-ess ()
  (with-eval-after-load 'ess-site
    (advice-add 'ess-set-style
                :after #'yxl-datascience/ess-set-style-advice)
    (yxl-datascience/ess-setup-generic)
    (yxl-datascience/ess-setup-bindings)
    (yxl-datascience/ess-setup-imenu)
    (yxl-datascience/ess-setup-rdired)
    (yxl-datascience/ess-setup-help)
    (yxl-datascience/ess-setup-lintr)
    (add-hook 'ess-mode-hook #'yxl-datascience/ess-hook)
    (add-hook 'R-mode-hook #'yxl-datascience/R-hook)
    (add-hook 'ess-mode-hook 'smartparens-mode)
    (add-hook 'ess-mode-hook 'fci-mode)
    (add-hook 'ess-mode-hook 'hl-todo-mode)
    (mapcar #'yxl-datascience/ess-set-leader-keys
            '(ess-mode ess-julia-mode inferior-ess-mode))
    (mapcar #'yxl-datascience/ess-declare-prefix
            '(ess-mode ess-julia-mode inferior-ess-mode))
    (yxl-datascience/setup-julia-bindings)))

(defun yxl-datascience/init-yxl-ess ()
  (use-package yxl-ess
    :after ess-site))

(defun yxl-datascience/init-ess-goodies ()
  (use-package ess-goodies
    :after ess-site))

(defun yxl-datascience/init-ess-view ()
  (use-package ess-view
    :after ess-site))

(defun yxl-datascience/post-init-markdown-mode ()
  (with-eval-after-load 'markdown-mode
    (progn
      (define-key markdown-mode-map (kbd "C-S-M")
        (lambda () (interactive)
          (yxl-insert-symbol "%>%"))))))

(defun yxl-datascience/post-init-org-mode ()
  (with-eval-after-load 'org
    (progn
      (define-key org-mode-map (kbd "C-S-M")
        (lambda () (interactive)
          (yxl-insert-symbol "%>%"))))))

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
