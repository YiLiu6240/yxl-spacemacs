(setq yxl-workspace-packages '((eyebrowse :location site)
                               winum
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
    :commands (eyebrowse-mode)
    :init
    (progn
      (setq eyebrowse-wrap-around t)
      (setq eyebrowse-keymap-prefix (kbd "C-c w"))
      (setq eyebrowse-default-tag-name "main")
      (eyebrowse-mode))
    :config
    (progn
      (setq eyebrowse-mode-line-style 'always)
      (setq eyebrowse-default-tag-name-list '((0 . "conf")
                                              (1 . "main")
                                              (2 . "support")
                                              (3 . "doc")
                                              (4 . "item")
                                              (8 . "info")
                                              (9 . "todo")))
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


(defun yxl-workspace/init-winum ()
  (use-package winum
    :config
    (progn
      (defun spacemacs//winum-assign-func ()
        "Custom number assignment for neotree."
        (when (and (boundp 'neo-buffer-name)
                   (string= (buffer-name) neo-buffer-name)
                   ;; in case there are two neotree windows. Example: when
                   ;; invoking a transient state from neotree window, the new
                   ;; window will show neotree briefly before displaying the TS,
                   ;; causing an error message. the error is eliminated by
                   ;; assigning 0 only to the top-left window
                   (eq (selected-window) (frame-first-window)))
          0))
      (setq winum-auto-assign-0-to-minibuffer nil
            winum-assign-func 'spacemacs//winum-assign-func
            winum-auto-setup-mode-line nil
            winum-ignored-buffers '(" *which-key*"))
      (spacemacs/set-leader-keys
        "0" 'winum-select-window-0-or-10
        "1" 'winum-select-window-1
        "2" 'winum-select-window-2
        "3" 'winum-select-window-3
        "4" 'winum-select-window-4
        "5" 'winum-select-window-5
        "6" 'winum-select-window-6
        "7" 'winum-select-window-7
        "8" 'winum-select-window-8
        "9" 'winum-select-window-9)
      (define-key winum-keymap (kbd "C-,")   'winum-select-window-by-number)
      (define-key winum-keymap (kbd "C-w 0") 'winum-select-window-0-or-10)
      (define-key winum-keymap (kbd "C-w 1") 'winum-select-window-1)
      (define-key winum-keymap (kbd "C-w 2") 'winum-select-window-2)
      (define-key winum-keymap (kbd "C-w 3") 'winum-select-window-3)
      (define-key winum-keymap (kbd "C-w 4") 'winum-select-window-4)
      (define-key winum-keymap (kbd "C-w 5") 'winum-select-window-5)
      (define-key winum-keymap (kbd "C-w 6") 'winum-select-window-6)
      (define-key winum-keymap (kbd "C-w 7") 'winum-select-window-7)
      (define-key winum-keymap (kbd "C-w 8") 'winum-select-window-8)
      (define-key winum-keymap (kbd "C-w 9") 'winum-select-window-9)
      (winum-mode))))

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
