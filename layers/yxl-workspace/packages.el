(setq yxl-workspace-packages '(eyebrowse
                               (yxl-ace-window :location site)
                               (yxl-session :location site)
                               buffer-move))

(defun yxl-workspace/init-yxl-ace-window ()
  (use-package yxl-ace-window
    :after (ace-window)
    :config
    (progn
      (with-eval-after-load 'counsel
        (ivy-add-actions 'counsel-find-file
                         '(("O" yxl-ace-window-open "dispatch to an ace window")
                           ("s" yxl-ace-window-open-horz "split horz, ace window")
                           ("v" yxl-ace-window-open-vert "split horz, ace window")))))))

(defun yxl-workspace/init-buffer-move ()
  (use-package buffer-move
    :defer t))

(defun yxl-workspace/init-eyebrowse ()
  (use-package eyebrowse
    :ensure t
    :init
    (eyebrowse-mode)
    :config
    (progn
      (setq eyebrowse-wrap-around t)
      (setq eyebrowse-keymap-prefix (kbd "C-c w"))
      (setq eyebrowse-default-tag-name "main")
      (setq eyebrowse-mode-line-style 'always)

      (defun yxl-eyebrowse-update-tag-name ()
        (interactive)
        (eyebrowse-rename-window-config
         (eyebrowse--get 'current-slot) (buffer-name)))

      (defun yxl-eyebrowse-rename-window-config (slot tag)
        "Rename the window config at SLOT to TAG.
When used interactively, default to the current window config,
use the prefix argument to prompt for a slot or a numerical
prefix argument to select a slot by its number."
        (interactive (list (cond
                            ((consp current-prefix-arg)
                             (eyebrowse--read-slot))
                            ((numberp current-prefix-arg)
                             current-prefix-arg)
                            (t (eyebrowse--get 'current-slot)))
                           nil))
        (let* ((window-configs (eyebrowse--get 'window-configs))
               (window-config (assoc slot window-configs))
               (current-tag (buffer-name))
               (tag (or tag (read-string "Tag: " current-tag))))
          (setf (nth 2 window-config) tag)))

      (advice-add 'eyebrowse-rename-window-config :override
                  #'yxl-eyebrowse-rename-window-config)

      (yxl-workspace/setup-eyebrowse)
      (eyebrowse-setup-opinionated-keys)
      ;; vim-style tab switching
      (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
      (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config)
      ;; overwrite gc
      (define-key evil-motion-state-map "gc" 'evilnc-comment-operator)
      (add-to-list 'window-persistent-parameters '(window-side . writable))
      (add-to-list 'window-persistent-parameters '(window-slot . writable))
      (define-key eyebrowse-mode-map (kbd "C-c w .")
        #'spacemacs/workspaces-transient-state/body)
      (define-key eyebrowse-mode-map (kbd "C-w .")
        #'spacemacs/workspaces-transient-state/body)
      (define-key eyebrowse-mode-map (kbd "C-w ,")
        #'eyebrowse-rename-window-config)
      (define-key eyebrowse-mode-map (kbd "C-c w C-h")
        #'eyebrowse-prev-window-config)
      (define-key eyebrowse-mode-map (kbd "C-c w C-l")
        #'eyebrowse-next-window-config)
      (define-key eyebrowse-mode-map (kbd "C-c w d")
        #'eyebrowse-close-window-config)
      (define-key eyebrowse-mode-map (kbd "C-c w R")
        #'eyebrowse-rename-window-config)
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

(defun yxl-workspace/init-yxl-session ()
  (use-package yxl-session
    :defer t
    :commands (yxl-session-load-1
               yxl-session-load-2
               yxl-session-save-1
               yxl-session-save-2)
    :config
    (progn
      (setq yxl-session-location "~/Dropbox/inbox/"))))
