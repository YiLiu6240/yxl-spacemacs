(setq yxl-evil-packages '(evil
                          evil-evilified-state
                          (yxl-evil :location site)
                          evil-surround
                          evil-textobj-column
                          evil-mc
                          evil-evilified-state
                          (evil-lion :location (recipe
                                                :fetcher github
                                                :repo "edkolev/evil-lion"))))

(defun yxl-evil/post-init-evil ()
  (with-eval-after-load 'evil
    (yxl-evil/setup-evil-main)
    (yxl-evil/setup-evil-personal)
    (yxl-evil/setup-evil-misc)
    ;; TODO: check if this causes trouble
    (setq evil-move-beyond-eol nil)))

(defun yxl-evil/post-init-evil-evilified-state ()
  (with-eval-after-load 'evil-evilified-state
    (yxl-evil/setup-evilified)
    (yxl-evil/setup-evilified-personal)))

(defun yxl-evil/init-yxl-evil ()
  (use-package yxl-evil
    :after evil))

(defun yxl-evil/post-init-evil-surround ()
  (with-eval-after-load 'evil-surround
    (add-hook 'prog-mode-hook #'yxl-evil/evil-surround-pairs)
    (add-hook 'text-mode-hook #'yxl-evil/evil-surround-pairs)
    (add-hook 'ein:notebook-multilang-mode-hook #'yxl-evil/evil-surround-pairs)))

(defun yxl-evil/init-evil-textobj-column ()
  (use-package evil-textobj-column))

(defun yxl-evil/post-init-evil-mc ()
  ;; https://github.com/TheBB/spacemacs-layers/blob/master/init.el
  (add-hook 'prog-mode-hook #'turn-on-evil-mc-mode)
  (add-hook 'text-mode-hook #'turn-on-evil-mc-mode)
  (add-hook 'evil-mc-after-cursors-deleted
            (defun bb/clear-anzu ()
              (interactive)
              (setq anzu--state nil))))

(defun yxl-evil/init-evil-lion ()
  (use-package evil-lion
    :ensure t
    :config
    (progn
      (setq evil-lion-left-align-key (kbd "ga"))
      (setq evil-lion-left-align-key (kbd "gA"))
      (evil-lion-mode))))
