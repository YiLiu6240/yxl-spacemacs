(setq yxl-ess-packages '(ess
                         (yxl-ess :location site)
                         (ess-goodies :location site)))

(defun yxl-ess/post-init-ess ()
  (with-eval-after-load 'ess-site
    (ess--unset-smart-S-assign-key)
    (add-hook 'ess-mode-hook 'smartparens-mode)
    (add-hook 'ess-mode-hook 'fci-mode)
    (add-hook 'ess-mode-hook 'hl-todo-mode)
    (add-hook 'ess-mode-hook 'which-function-mode)
    (yxl-ess/ess-setup)
    (yxl-ess/ess-setup-imenu)
    (add-hook 'R-mode-hook #'yxl-ess/R-hook)
    (key-chord-define ess-mode-map ">>" "%>%"))

  (with-eval-after-load 'flycheck
   (setq flycheck-lintr-linters
        (concat "with_defaults(assignment_linter=NULL, "
                "camel_case_linter=NULL, "
                "commented_code_linter=NULL, "
                "infix_spaces_linter=NULL)"))))

(defun yxl-ess/init-yxl-ess ()
  (use-package yxl-ess
    :after ess-site))

(defun yxl-ess/init-ess-goodies ()
  (use-package ess-goodies
    :after ess-site))
