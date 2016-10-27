(setq yxl-ess-packages '(ess
                         (ess-goodies :location local)))

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

    ;; ess mode leader key bindings
    (spacemacs/set-leader-keys-for-major-mode 'ess-mode
      "sr" #'ess-eval-region-and-newline)
    (spacemacs/set-leader-keys-for-major-mode 'ess-mode
      ",e" #'ess-execute
      ",d" #'ess-rdired
      ",s" #'ess-at-point-str
      ",S" #'ess-at-point-generic
      ",fs" #'yxl-ess/lsos
      ",ff" #'yxl-ess/lsdf
      ",h" #'ess-help
      ",i" #'asb-ess-R-object-popup-str
      ",I" #'asb-ess-R-object-popup-interactive)
    (add-hook 'ess-rdired-mode-hook
              (lambda ()
                (local-set-key "s" 'ess-rdired-str)
                (local-set-key "S" 'ess-rdired-sort))))

  (with-eval-after-load 'flycheck
   (setq flycheck-lintr-linters
        (concat "with_defaults(assignment_linter=NULL, "
                "camel_case_linter=NULL, "
                "commented_code_linter=NULL, "
                "infix_spaces_linter=NULL)"))))

(defun yxl-ess/init-ess-goodies ()
  (use-package ess-goodies
    :after ess-site))
