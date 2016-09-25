(setq yxl-org-packages '(org))

(defun yxl-org/post-init-org ()
  ;; misc settings
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook 'smartparens-mode)
    (with-eval-after-load 'evil-org
     (evil-define-key 'normal evil-org-mode-map
       "-" 'dired-jump
       "_" 'projectile-dired)))

  ;; inject my own configs
  (with-eval-after-load 'org
    (yxl-org/config-general)
    (yxl-org/config-capture)
    (yxl-org/config-keywords)
    (yxl-org/config-agenda)))
