(setq yxl-org-packages '(org
                         (evil-org :location local)))

(defun yxl-org/post-init-org ()
  ;; misc settings
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook 'smartparens-mode)
    ;; (add-hook 'org-mode-hook 'org-bullets-mode)
    (add-hook 'org-mode-hook 'yxl-org/org-mode-hook)
    (with-eval-after-load 'evil-org
     (evil-define-key 'normal evil-org-mode-map
       "-" 'dired-jump
       "_" 'projectile-dired)))

  ;; inject my own configs
  (with-eval-after-load 'org
    (yxl-org/setup-general)
    (yxl-org/setup-capture)
    (yxl-org/setup-keywords)
    (yxl-org/setup-agenda)))

(defun yxl-org/init-evil-org ()
  (use-package evil-org
    ;; :commands (evil-org-mode evil-org-recompute-clocks)
    ;; :init (add-hook 'org-mode-hook 'evil-org-mode)
    :defer t
    :after 'org
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "C" 'evil-org-recompute-clocks))))
