(setq yxl-evil-packages '(evil
                          evil-evilified-state
                          (yxl-evil :location site)
                          evil-surround
                          evil-textobj-column
                          evil-mc
                          evil-evilified-state))

(defun yxl-evil/post-init-evil ()
  (with-eval-after-load 'evil
    (setq evil-split-window-below t)
    (setq evil-vsplit-window-right t)
    ;; TODO: check if this causes trouble
    (setq evil-move-beyond-eol nil)
    ;; stay in normal mode when switch to wdired-mode
    (delete 'wdired-mode evil-insert-state-modes)
    (yxl-evil//setup-evil-general)
    (yxl-evil//setup-evil-insert-state)
    (yxl-evil//setup-evil-c-hjkl)
    (yxl-evil//setup-evil-text-objects)
    (evil-ex-define-cmd "Evimrc" #'evil-Evimrc)
    (with-eval-after-load 'eyebrowse
      (yxl-evil//setup-evil-eyebrowse))))

(defun yxl-evil/post-init-evil-evilified-state ()
  (with-eval-after-load 'evil-evilified-state
    (yxl-evil//setup-evilified-general)
    (yxl-evil//setup-evilified-c-hjkl)))

(defun yxl-evil/init-yxl-evil ()
  (use-package yxl-evil
    :after evil
    :config
    (progn
      (define-key evil-motion-state-map
        "zz" 'yxl-evil-scroll-line-to-golden-ratio))))

(defun yxl-evil/post-init-evil-surround ()
  (with-eval-after-load 'evil-surround
    (add-hook 'prog-mode-hook #'yxl-evil//evil-surround-pairs)
    (add-hook 'text-mode-hook #'yxl-evil//evil-surround-pairs)
    (add-hook 'ein:notebook-multilang-mode-hook #'yxl-evil//evil-surround-pairs)))

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
