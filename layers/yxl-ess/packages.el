(setq yxl-ess-packages '(ess
                         (yxl-ess :location site)
                         (ess-goodies :location site)
                         ess-view
                         polymode))

(defun yxl-ess/post-init-ess ()
  (with-eval-after-load 'ess-site
    (advice-add 'ess-set-style :after #'yxl-ess/ess-set-style-advice)
    (yxl-ess/setup-generic)
    (yxl-ess/setup-bindings)
    (yxl-ess/setup-imenu)
    (yxl-ess/setup-rdired)
    (yxl-ess/setup-lintr)
    (add-hook 'ess-mode-hook #'yxl-ess/ess-hook)
    (add-hook 'R-mode-hook #'yxl-ess/R-hook)
    (add-hook 'ess-mode-hook 'smartparens-mode)
    (add-hook 'ess-mode-hook 'fci-mode)
    (add-hook 'ess-mode-hook 'hl-todo-mode)
    (mapcar #'yxl-ess/set-leader-keys '(ess-mode inferior-ess-mode))
    (mapcar #'yxl-ess/declare-prefix '(ess-mode inferior-ess-mode))))

(defun yxl-ess/init-yxl-ess ()
  (use-package yxl-ess
    :after ess-site))

(defun yxl-ess/init-ess-goodies ()
  (use-package ess-goodies
    :after ess-site))

(defun yxl-ess/init-ess-view ()
  (use-package ess-view
    :after ess-site))

(defun yxl-ess/init-polymode ()
  (use-package polymode
    :mode (("\\.Rmd"   . Rmd-mode))
    :init
    (progn
      (defun Rmd-mode ()
        "ESS Markdown mode for Rmd files"
        (interactive)
        (require 'poly-R)
        (require 'poly-markdown)
        (R-mode)
        (poly-markdown+r-mode)))))
