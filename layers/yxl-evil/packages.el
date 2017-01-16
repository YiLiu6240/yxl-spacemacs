(setq yxl-evil-packages '(evil
                          evil-evilified-state
                          (yxl-evil :location site)
                          evil-surround
                          evil-textobj-column
                          evil-mc
                          evil-evilified-state))

(defun yxl-evil/post-init-evil ()
  (with-eval-after-load 'evil
    (yxl-evil/setup-evil-main)
    (yxl-evil/setup-evil-personal)
    (yxl-evil/setup-evil-misc)))

(defun yxl-evil/post-init-evil-evilified-state ()
  (with-eval-after-load 'evil-evilified-state
    (yxl-evil/setup-evilified)
    (yxl-evil/setup-evilified-personal)))

(defun yxl-evil/init-yxl-evil ()
  (use-package yxl-evil
    :load-path "~/dotfiles/yxl-emacs-goodies/yxl-evil"
    :after evil))

(defun yxl-evil/post-init-evil-surround ()
  (with-eval-after-load 'evil-surround
    (add-hook 'prog-mode-hook #'yxl-evil/evil-surround-pairs)
    (add-hook 'text-mode-hook #'yxl-evil/evil-surround-pairs)))

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
