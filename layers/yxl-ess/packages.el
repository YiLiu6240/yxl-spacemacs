(setq yxl-ess-packages '(ess
                         (yxl-ess :location site)
                         (ess-goodies :location site)))

(defun yxl-ess/post-init-ess ()
  (with-eval-after-load 'ess-site
    (ess--unset-smart-S-assign-key)
    (add-hook 'ess-mode-hook 'smartparens-mode)
    (add-hook 'ess-mode-hook 'fci-mode)
    (add-hook 'ess-mode-hook 'hl-todo-mode)
    (yxl-ess/setup)
    (yxl-ess/setup-imenu)
    (yxl-ess/setup-rdired)
    (yxl-ess/setup-lintr)
    (add-hook 'R-mode-hook #'yxl-ess/R-hook)
    (mapcar #'yxl-ess/set-leader-keys '(ess-mode inferior-ess-mode))
    (mapcar #'yxl-ess/declare-prefix '(ess-mode inferior-ess-mode))
    (key-chord-define ess-mode-map ">>" "%>%")
    (key-chord-define ess-mode-map "__" "<-")))

(defun yxl-ess/init-yxl-ess ()
  (use-package yxl-ess
    :after ess-site))

(defun yxl-ess/init-ess-goodies ()
  (use-package ess-goodies
    :after ess-site))
