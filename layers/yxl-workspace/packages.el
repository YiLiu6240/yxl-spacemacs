(setq yxl-workspace-packages '((eyebrowse :location local)
                               buffer-move))

(defun yxl-workspace/init-buffer-move ()
  (use-package buffer-move
    :defer t))

(defun yxl-workspace/init-eyebrowse ()
  (use-package eyebrowse
    :commands (eyebrowse-mode)
    :init
    (progn
      (setq eyebrowse-wrap-around t)
      (setq eyebrowse-keymap-prefix (kbd "C-c w"))
      (eyebrowse-mode))
    :config
    (progn
      (setq eyebrowse-mode-line-style 'always)  ;; display all configs
      (yxl-workspace/setup-eyebrowse)
      (eyebrowse-setup-opinionated-keys)
      ;; vim-style tab switching
      (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
      (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config)
      ;; overwrite gc
      (define-key evil-motion-state-map "gc" 'evilnc-comment-operator)
      (add-to-list 'window-persistent-parameters '(window-side . writable))
      (add-to-list 'window-persistent-parameters '(window-slot . writable))
      (define-key eyebrowse-mode-map (kbd "C-c w d")
        #'eyebrowse-close-window-config)
      ;; eyebrowse new window config:
      ;; c: jump to current dired
      ;; C: clone current window config
      ;; C-c: new config with current window maximized
      (setq eyebrowse-new-workspace 'dired-stay-or-jump)
      (define-key eyebrowse-mode-map (kbd "C-c w c")
        #'eyebrowse-create-window-config-dired)
      (define-key eyebrowse-mode-map (kbd "C-c w C")
        #'eyebrowse-create-window-config-clone)
      (define-key eyebrowse-mode-map (kbd "C-c w C-c")
        #'eyebrowse-create-window-config-main))))
