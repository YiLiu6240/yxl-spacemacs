(setq yxl-jupyter-packages '(ein))

(defun yxl-jupyter/init-ein ()
  (use-package ein
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "ajn" #'ein:notebooklist-open)
      (spacemacs/set-leader-keys "ajN" #'ein:notebooklist-login)
      (spacemacs/declare-prefix "aj" "jupyter-notebook"))
    :config
    (progn
      (setq spacemacs-useful-buffers-regexp
            (append spacemacs-useful-buffers-regexp '("\\*ein:*")))
      (setq ein:completion-backend 'ein:use-company-backend)
      (with-eval-after-load 'ein-notebook
        (advice-add 'ein:notebook-save-notebook
                    :override #'ein:notebook-save-notebook-override))
      (with-eval-after-load 'ein-multilang
        (add-hook 'ein:notebook-multilang-mode-hook
                  #'smartparens-mode))
      (with-eval-after-load 'ein-cell-edit
        (advice-add 'ein:edit-cell-exit
                    :override #'ein:edit-cell-exit-override))
      ;; no idea why we have to enforce evilified state on pager to work
      (with-eval-after-load 'ein-pager
        (add-hook 'ein:pager-mode-hook #'evil-evilified-state))
      (yxl-jupyter/setup-general-keybindings)
      (yxl-jupyter/setup-evilified-keybindings)
      (yxl-jupyter/setup-leader-keys)
      (yxl-jupyter/setup-hydra))))
