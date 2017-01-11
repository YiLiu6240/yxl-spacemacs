(setq yxl-org-packages '(org
                         (yxl-org-patch :location local)
                         (evil-org :location local)
                         org-gcal))

(defun yxl-org/post-init-org ()
  ;; misc settings
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook 'smartparens-mode)
    ;; (add-hook 'org-mode-hook 'org-bullets-mode)
    (add-hook 'org-mode-hook 'yxl-org/org-mode-hook))
  ;; inject my own configs
  (with-eval-after-load 'org
    (yxl-org/setup-general)
    (yxl-org/setup-bindings)
    (yxl-org/setup-capture)
    (yxl-org/setup-keywords)
    (yxl-org/setup-agenda)
    (yxl-org/setup-babel)))

(defun yxl-org/init-yxl-org-patch ()
  (use-package yxl-org-patch
    :after (org)))

(defun yxl-org/init-evil-org ()
  (use-package evil-org
    ;; :commands (evil-org-mode evil-org-recompute-clocks)
    ;; :init (add-hook 'org-mode-hook 'evil-org-mode)
    :defer t
    :after 'org
    :config
    (progn
      (evil-define-key 'normal evil-org-mode-map
        "O" 'evil-open-above)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "C" 'evil-org-recompute-clocks))))

(defun yxl-org/init-org-gcal ()
  (use-package org-gcal
    :defer t
    :commands (org-gcal-sync)
    :config
    (progn
      (setq package-check-signature nil)
      (setq org-gcal-file-alist
            '(("yi.liu6240@gmail.com" .  "~/Dropbox/org/tasks/cal_google.org")))
      (defun my-org-gcal-notify (title mes)
        (message "org-gcal::%s - %s" title mes))
      (fset 'org-gcal--notify 'my-org-gcal-notify))))
